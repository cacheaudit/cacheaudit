open AD.DS
open NumAD.DS
open Logger

type replacement_strategy = 
  | LRU  (** least-recently used *)
  | FIFO (** first in, first out *)
  | PLRU (** tree-based pseudo LRU *)

type cache_param = { 
  cs: int; 
  ls: int; 
  ass: int; 
  str: replacement_strategy;
 } 


module type S = sig
  include AD.S
  val init : cache_param -> t
  (** initialize an empty cache
   takes arguments cache_size (in bytes), 
  line_size (in bytes) and associativity *)
  val touch : t -> int64 -> rw_t -> t
  (** reads or writes an address into cache *)

  val touch_hm : t -> int64 -> rw_t -> (t add_bottom*t add_bottom)
  (** Same as touch, but returns more precise informations about hit and misses *)
  (** @return, the first set overapproximates hit cases, the second one misses *)
  val elapse : t -> int -> t
  (** Used to keep track of time, if neccessary *)
  val count_cache_states : t -> Big_int.big_int
end


open Big_int 

(* if following flags are true, cache updates will be done by concretizing *)
(* (for a cache set), performing update and abstracting, upon*)
(* cache hits, resp. misses*)
let do_concrete_miss = ref false
let do_concrete_hit = ref false


(* the following flag turns on an optimization for LRU/FIFO when doing *)
(* abstract update (flag above is false) *)
let opt_touch_precision = ref true

(* This flag can disable an optimization (for precision) *)
(* which checks whether ages of elements are achievable by the *)
(* corresponding replacement strategy.*)
(* If disabled, holes will be counted as possible, if enabled, only the holes*)
(* achievable by the strategy will be counted *)
let exclude_impossible = ref true

type adversay = Blurred | SharedSpace

let adversary = ref Blurred

module Make (A: AgeAD.S) = struct
  (* Permutation to apply when touching an element of age a in PLRU *)
  (* We assume an ordering correspond to the boolean encoding of the tree from *)
  (* leaf to root (0 is the most recent, corresponding to all 0 bits is the path *)
  let plru_permut assoc a n = if n=assoc then n else
    let rec f a n =  
      if a=0 then n 
      else 
        if a land 1 = 1 then 
  	if n land 1 = 1 then 2*(f (a/2) (n/2)) 
  	else n+1
        else (* a even*) 
  	if n land 1 = 1 then n 
  	else 2*(f (a/2) (n/2))
    in f a n
  
  
  let lru_permut assoc a n = 
    if n = a then 0
    else if n < a then n+1
    else n
  
  let fifo_permut assoc a n = n
  
  let get_permutation strategy = match strategy with
    | LRU -> lru_permut
    | FIFO -> fifo_permut
    | PLRU -> plru_permut
   
  let intset_map f iset = 
    IntSet.fold (fun x st -> IntSet.add (f x) st) iset IntSet.empty
  
  (* Return a set containing sets of possible age allocations within a cache set *)
  (* depending on the replacement strategy *)
  let calc_poss_ages strategy assoc = 
    let permut = get_permutation strategy in
    let rec loop ready todo = 
      if IntSetSet.is_empty todo then ready
      else 
        let elt = IntSetSet.choose todo in
        let ready = IntSetSet.add elt ready in
        let todo = IntSetSet.remove elt todo in
        (* hit successors *)
        let successors = IntSet.fold (fun i succ ->
          IntSetSet.add (intset_map (permut assoc i) elt) succ
          ) elt IntSetSet.empty in
        (* miss successor *)
        let miss_elt = IntSet.remove assoc (IntSet.add 0 (intset_map succ elt)) in
        let successors = IntSetSet.add miss_elt successors in
        let todo = IntSetSet.diff (IntSetSet.union todo successors) ready in
        loop ready todo in
    loop IntSetSet.empty (IntSetSet.singleton IntSet.empty) 
  
  
  type t = {
    handled_addrs : NumSet.t; (* holds addresses handled so far *)
    cache_sets : NumSet.t IntMap.t;
    (* holds a set of addreses which fall into a cache set
        as implemented now it may also hold addresses evicted from the cache *)
    ages : A.t;
    (* for each accessed memory address holds its possible ages *)

    cache_size: int;
    line_size: int; (* same as "data block size" *)
    assoc: int;
    num_sets : int; (* computed from the previous three *)
    
    strategy : replacement_strategy;
    poss_ages : IntSetSet.t
  }
  
    let var_to_string x = Printf.sprintf "%Lx" x 
  
  let calc_set_addr num_sets addr = 
    Int64.to_int (Int64.rem addr (Int64.of_int num_sets))

  (* Determine the set in which an address is cached *)
  let get_set_addr env addr =
    calc_set_addr env.num_sets addr
    
  let init cache_param =
    let (cs,ls,ass,strategy) = (cache_param.cs, cache_param.ls,
      cache_param.ass,cache_param.str) in
    let ns = cs / ls / ass in (* number of sets *)
    let rec init_csets csets i = match i with
    | 0 -> csets
    | n -> init_csets (IntMap.add (n - 1) NumSet.empty csets) (n - 1) in
    { cache_sets = init_csets IntMap.empty ns;
      ages = A.init ass (calc_set_addr ns) var_to_string;
      handled_addrs = NumSet.empty;
      cache_size = cs;
      line_size = ls;
      assoc = ass;
      num_sets = ns;
      strategy = strategy;
      poss_ages = calc_poss_ages strategy ass
    }
  
  (* Gives the block address *)
  let get_block_addr env addr = Int64.div addr (Int64.of_int env.line_size)
  
  
  (*** Counting valid states ***)
  
  (* check whether cstate is a possible concretization for a cache set,*)
  (* according to the list poss_ages containing the possible ages in the cache set*)
  let is_poss poss_ages cstate = 
    let state_ages,_ = List.fold_left (fun (st,ctr) x -> 
        match x with
        | None -> (st,succ ctr)
        | Some _ -> (IntSet.add ctr st, succ ctr)
      ) (IntSet.empty,0) cstate in
    IntSetSet.mem state_ages poss_ages
    
  
  (* Checks if the given cache state is valid *)
  (* with respect to the ages (which are stored in [env])*)
  (*  of the elements of the same cache set [addr_set]*)
  let is_valid_cstate env addr_set cache_state = 
    assert (List.for_all (function Some a -> NumSet.mem a addr_set | None -> true) cache_state);
    if !exclude_impossible && (not (is_poss env.poss_ages cache_state)) then false
    else
      (* get the position of [addr] in cache state [cstate], starting from [i];*)
      (* if the addres is not in cstate, then it should be assoc *)
      let rec pos addr cstate i = match cstate with 
         [] -> env.assoc
      | hd::tl -> if hd = Some addr then i else pos addr tl (i+1) in
      NumSet.for_all (fun addr -> 
        List.mem (pos addr cache_state 0) (A.get_values env.ages addr)) addr_set 
  
  
  (* create a uniform list containing n times the element x *)
  let create_ulist n x =
    let rec loop n x s = 
      if n <= 0 then s else loop (n-1) x (x::s)
    in loop n x []
  
  (* create a list with the elements [a;a+1;...;b-1] (not including b) *)
  let create_range a b = 
    let rec loop x s = 
      if x < a then s else loop (x-1) (x::s)
    in loop (b-1) []
  
  module NumSetSet = Set.Make(NumSet)
  let numset_from_list l =
    List.fold_left (fun set elt -> 
      match elt with None -> set
      | Some i -> NumSet.add i set) NumSet.empty l
  
  (* Count the number of n-permutations of the address set addr_set*)
  (* which are also a valid cache state *)
  let num_tuples env n addr_set = 
    (* Preprocessing step for counting: set of all possible sets of ages*)
    (* of the blocks within a cache set *)
    if NumSet.cardinal addr_set >= n then begin
      (* the loop creates all n-permutations and tests each for validity *)
      let rec loop n elements tuple num = 
        if n = 0 then 
            let rec loop_holes cstate rem_elts num = 
              if (List.length rem_elts) = 0 then 
                (* no rem_elts -> this is a possible cache state;*)
                (* check it for validity *)
                if is_valid_cstate env addr_set cstate then Int64.add num 1L else num
              else
                let elt = List.hd rem_elts in
                let rem_elts = List.tl rem_elts in
                (* a list containing the possible number of holes before elt *)
                let poss_num_holes = create_range 0 (env.assoc - 
                  (List.length cstate) - (List.length rem_elts)) in
                
                List.fold_left (fun num nholes -> 
                  (* add the nholes holes and elt to the state *)
                  (* and continue scanning the remaining elements *)
                  loop_holes (cstate @ (create_ulist nholes None) @ [elt]) rem_elts num
                  ) num poss_num_holes
            in loop_holes [] tuple num
        else
          (* add next element to tuple and continue looping *)
          (* (will go on until n elements have been picked) *)
          NumSet.fold (fun addr s1 -> 
            loop (n-1) (NumSet.remove addr elements) ((Some addr)::tuple) s1
            ) elements num in 
      loop n addr_set [] 0L
    end else 0L
  
  (* Computes two lists where each item i is the number of possible *)
  (* cache states of cache set i for a shared-memory *)
  (* and the disjoint-memory (blurred) adversary *)
  let cache_states_per_set env =
    let cache_sets = Utils.partition (A.var_names env.ages) (get_set_addr env) in
    IntMap.fold (fun _ addr_set (nums,bl_nums) ->
        let num_tpls,num_bl =
          let rec loop i (num,num_blurred) =
            if i > env.assoc then (num,num_blurred)
            else
              let this_num = 
                num_tuples env i addr_set in
              let this_bl = if this_num = 0L then 0L else 1L in
              loop (i+1) (Int64.add num this_num, Int64.add num_blurred this_bl) in 
          loop 0 (0L,0L) in 
        (num_tpls::nums,num_bl::bl_nums)
      ) cache_sets ([],[])
      
  let count_cstates env = 
    let nums_cstates,bl_nums_cstates = cache_states_per_set env in
      (Utils.prod nums_cstates,Utils.prod bl_nums_cstates)
      
      
  (*** Printing ***)
  
  let print_addr_set fmt = NumSet.iter (fun a -> Format.fprintf fmt "%Lx " a)

  let count_cache_states env = 
    let num_cstates,bl_num_cstates = count_cstates env in
    match !adversary with
    | Blurred -> bl_num_cstates
    | SharedSpace -> num_cstates
  
  (* [print num] prints [num], which should be positive, as well as how many
     bits it is. If [num <= 0], print an error message *)
  let print_num fmt num =
    let strnum = string_of_big_int num in
    if gt_big_int num zero_big_int then 
      Format.fprintf fmt "%s, (%f bits)\n" strnum (Utils.log2 num)
    else begin
      Format.fprintf fmt "counting not possible\n";
      if get_log_level CacheLL = Debug then
        Format.fprintf fmt  "Number of configurations %s\n" strnum;
    end
  
  let print fmt env =
      Format.fprintf fmt "Final cache state:\n";
      Format.fprintf fmt "@[Set: addr1 in {age1,age2,...} addr2 in ...@.";
      IntMap.iter (fun i all_elts ->
          if not (NumSet.is_empty all_elts) then begin
            Format.fprintf fmt "@;%3d: " i;
            NumSet.iter (fun elt -> 
              Format.fprintf fmt "%Lx" elt;
              Format.fprintf fmt " in {%s} @,"
                (String.concat "," (List.map
                  string_of_int (A.get_values env.ages elt)))
              ) all_elts;
            Format.fprintf fmt "@]"
          end
        ) env.cache_sets;
    Format.printf "\n";
    let num, bl_num = count_cstates env in
    Format.fprintf fmt "\nNumber of valid cache configurations: ";
    print_num fmt num;
    Format.fprintf fmt "\nNumber of valid cache configurations (blurred): ";
    print_num fmt bl_num
  

  (* Removes a cache line when we know it cannot be in the cache *)

  let remove_block env addr =
    let addr_set = get_set_addr env addr in
    let cset = IntMap.find addr_set env.cache_sets in
    let cset = NumSet.remove addr cset in
    let handled_addrs = NumSet.remove addr env.handled_addrs in
    { env with
      ages = A.delete_var env.ages addr;
      handled_addrs = handled_addrs;
      cache_sets = IntMap.add addr_set cset env.cache_sets;
    }
  

  let join c1 c2 =
    assert ((c1.assoc = c2.assoc) && 
      (c1.num_sets = c2.num_sets));
    let handled_addrs = NumSet.union c1.handled_addrs c2.handled_addrs in
    let cache_sets = IntMap.merge 
      (fun k x y ->
        match x,y with
        | Some cset1, Some cset2 ->
           Some (NumSet.union cset1 cset2)
        | Some cset1, None -> Some cset1
        | None, Some cset2 -> Some cset2
        | None, None -> None 
      ) c1.cache_sets c2.cache_sets in
    let assoc = c1.assoc in
    let haddr_1minus2 = NumSet.diff c1.handled_addrs c2.handled_addrs in
    let haddr_2minus1 = NumSet.diff c2.handled_addrs c1.handled_addrs  in
    (* add missing variables to ages *)
    let ages1 = NumSet.fold (fun addr c_ages ->
      A.set_var c_ages addr assoc) haddr_2minus1 c1.ages in
    let ages2 = NumSet.fold (fun addr c_ages ->
      A.set_var c_ages addr assoc) haddr_1minus2 c2.ages in
    let ages = A.join ages1 ages2 in
    { c1 with ages = ages; handled_addrs = handled_addrs;
    cache_sets = cache_sets}
  
  let remove_not_cached env block =
    if (A.get_values env.ages block) = [env.assoc] then
      remove_block env block
    else env

  let get_cset env block = 
    let set_addr = get_set_addr env block in
    IntMap.find set_addr env.cache_sets

  (* Remove the "add_bottom" *)
  let strip_bot = function
    | Bot -> raise Bottom
    | Nb x -> x
  
  
  (* [i -- j] returns the list [i; i+1; ... ; j-1]*)
  let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux (j-1) []
  
  (* Returns a list of concretizations corresponding to one cache set [cset]. *)
  (* Each concretization is a NumMap, mapping blocks to ages *)
  let concretize_set env cset =
    let blocks = NumSet.elements cset in
    let abstraction = List.map (fun blck -> 
        A.get_values env.ages blck
      ) blocks in
    let lens = List.map List.length abstraction in
    let num_concrete = List.fold_left (fun x prod -> prod * x) 1 lens in
    let pos i n = 
      let rec divide x n =  
         if n = 0 then x
         else divide (x/(List.nth lens (n-1))) (n-1) in
      (divide i (n)) mod (List.nth lens n) in
    
    List.fold_left (fun concr i -> 
        let state = 
          List.fold_left (fun state n -> 
            NumMap.add (List.nth blocks n) 
              (List.nth (List.nth abstraction n) (pos i n)) state
           ) NumMap.empty (0 -- (List.length blocks)) in
        state :: concr
        ) [] (0 -- num_concrete)
  
  let abstract_set env concr = 
    let abstr,_ = 
      List.fold_left (fun (ages,first_time) state -> 
        (* Set ages of current concrete state *)
        let nages = NumMap.fold (fun block age nages -> 
          A.set_var nages block age) state ages in 
        (* We overwrite the first value and join afterwards *)
        if first_time then (nages,false) else (A.join ages nages,false)
        ) (env.ages,true) concr in
    abstr
  
  (* The permutation belonging to a cache miss: age of accessed block is set *)
  (* to 0, ages of other blocks are incremented, unless if age = associativity *)
  let miss_permut assoc accessed_block this_block = 
    if this_block = accessed_block then fun _ -> 0
    else fun age -> if age = assoc then age else succ age
    
  (* The effect of one touch of addr, restricting to the case when addr
     is of age c. c=assoc corresponds to a miss, in which case the age
     of all blocks is incremented -- except for the touched block,
     whose age is set to 0. c < assoc corresponds to a hit, in which
     case a permutation is applied to the ages of all blocks *)
  let one_touch env block block_age rw = 
    let strategy = env.strategy in
    let cset = get_cset env block in
    let is_miss = block_age = env.assoc in 
    (* Comply to 'no write-allocate' policy: if there is a write-miss, *)
    (* do not put the element into cache *)
    if rw = Write && is_miss then env
    else if (is_miss && !do_concrete_miss) 
      || ((not is_miss) && !do_concrete_hit) then
        let env = if is_miss then env else
          (* make sure no other blocks in the set have ages block_age *)
          let ages = NumSet.fold
            (fun b new_ages ->
              if b = block then new_ages
              else
                let ages_young,ages_old = A.comp new_ages b block in
                (* One of the ages can be bottom, however not both *)
                strip_bot (lift_combine A.join ages_young ages_old)
            ) cset env.ages in
          {env with ages = A.set_var ages block block_age} in
        
        (* concretize *)
        let concr = concretize_set env cset in
        (* remove impossible *)
        let is_poss assoc state = 
          let state_ages = NumMap.fold (fun _ age age_set -> 
            IntSet.add age age_set) state IntSet.empty in
          (* assoc is always a possible age *)
          let state_ages = IntSet.remove assoc state_ages in
          IntSetSet.mem state_ages env.poss_ages in
        let concr = List.filter (is_poss env.assoc) concr in
        
        (* Permute values *)
        (* operation is left with non-tail-recursive List.map on purpose *)
        (* so that computation is disrupted for an impossibly long list *)
        let permut = if is_miss then miss_permut 
          else let perm_hit = get_permutation strategy in
          fun assoc _ _ -> perm_hit assoc block_age in
        let concr = List.map (NumMap.mapi (permut env.assoc block)) concr in
        (* abstract *)
        {env with ages = abstract_set env concr}
    else (* do abstract-level update *)
      if is_miss then
        (* abstract-level miss:*)
        (* set age to 0 and increment ages of other blocks *)
        let env = {env with ages = A.set_var env.ages block 0} in
        NumSet.fold (fun blck nenv -> 
          if blck = block then nenv else
            let nags = 
              let ags = nenv.ages in
              let nin_cache = List.mem env.assoc (A.get_values ags blck) in
              let ags = A.inc_var ags blck in
              if !opt_touch_precision && 
                ((strategy = LRU) || (strategy = FIFO)) then
                (* Optimization: disallow ages >= cardinality of cache set *)
                let ages_in_cache,_ = A.comp_with_val ags blck (NumSet.cardinal cset) in
                (* age "associativity" is still possible *)
                if nin_cache then 
                  let _,ages_nin_cache = A.comp_with_val ags blck env.assoc in
                  strip_bot (lift_combine A.join ages_in_cache ages_nin_cache)
                else 
                  strip_bot ages_in_cache
              else ags
            in {nenv with ages = nags}
            (* in remove_not_cached {nenv with ages = nags} blck *)
          ) cset env
      else (* abstract-level hit *)
        (* optimize for FIFO *)
        if strategy = FIFO then env 
        else
          (* Permute ages of all blocks != block *)
          let perm = get_permutation strategy in
          let nages = NumSet.fold
              (fun b new_ages ->
                  if b = block then new_ages
                  else
                    let permute_ages ages = match ages with
                      | Bot -> Bot
                      | Nb ags -> Nb (A.permute ags (perm env.assoc block_age) b) in 
                    let ages_young,ages_old = A.comp new_ages b block in
                    let ages_young = permute_ages ages_young in
                    let ages_old =
                      (* optimize for LRU *)
                      if strategy = LRU then ages_old
                      else permute_ages ages_old in
                    (* One of the ages can be bottom, however not both *)
                    strip_bot (lift_combine A.join ages_young ages_old)
              ) cset env.ages in
          (* Permute ages of block *)
          {env with ages = (A.permute nages (perm env.assoc block_age) block)}
  
  (* adds a new address handled by the cache if it's not already handled *)
  (* That works for LRU, FIFO and PLRU *)
  let add_new_address env block =
     (* the block has the default age of associativity *)
     let ages = A.set_var env.ages block env.assoc in
    let set_addr = get_set_addr env block in
    let cset = get_cset env block in
     let h_addrs = NumSet.add block env.handled_addrs in
     let cache_sets = IntMap.add set_addr (NumSet.add block cset) env.cache_sets in
     {env with ages = ages; handled_addrs = h_addrs; cache_sets = cache_sets}
  
  
  (* retuns true if block was handled *)
  let is_handled env block = 
    NumSet.mem block env.handled_addrs
  
  (* Reads or writes an address into cache *)
  let touch env orig_addr rw =
    if get_log_level CacheLL = Debug then Printf.printf "\nWriting cache %Lx" orig_addr;
    (* we cache the block address *)
    let block = get_block_addr env orig_addr in
    if get_log_level CacheLL = Debug then Printf.printf " in block %Lx\n" block;
    let env = if is_handled env block then env 
      else add_new_address env block in
    let block_ages = A.get_values env.ages block in
    (try
    let new_env = List.fold_left 
      (fun nenv block_age ->
        match A.exact_val env.ages block block_age with
        | Bot -> raise Bottom
        | Nb xages -> lift_combine join nenv 
          (Nb (one_touch {env with ages = xages} block block_age rw))
      ) Bot block_ages in
    strip_bot new_env
    with Bottom -> assert false) (* Touch shouldn't produce bottom *)

  (* Same as touch, but returns two possible configurations, one for the hit and the second for the misses *)
  let touch_hm env orig_addr rw = 
    let block = get_block_addr env orig_addr in
    let env = if is_handled env block then env 
      else add_new_address env block in
    (* ages_in is the set of ages for which there is a hit *)
    let ages_in, ages_out = 
        A.comp_with_val env.ages block env.assoc in
    let t a = match a with Bot -> Bot 
              | Nb a -> Nb(touch {env with ages=a} orig_addr rw)
    in
    (t ages_in, t ages_out)

  let widen c1 c2 = 
    join c1 c2

  let subseteq c1 c2 =
    assert 
      ((c1.assoc = c2.assoc) && (c1.num_sets = c2.num_sets));
    (NumSet.subset c1.handled_addrs c2.handled_addrs) &&
    (A.subseteq c1.ages c2.ages) &&
    (IntMap.for_all (fun addr vals ->
      if IntMap.mem addr c2.cache_sets
      then NumSet.subset vals (IntMap.find addr c2.cache_sets)
      else false
     ) c1.cache_sets)

      
  let print_delta c1 fmt c2 = match get_log_level CacheLL with
    | Debug->
      let added_blocks = NumSet.diff c2.handled_addrs c1.handled_addrs
      and removed_blocks = NumSet.diff c1.handled_addrs c2.handled_addrs in
      if not (NumSet.is_empty added_blocks) then Format.fprintf fmt
        "Blocks added to the cache: %a@;" print_addr_set added_blocks;
      if not (NumSet.is_empty removed_blocks) then Format.fprintf fmt
        "Blocks removed from the cache: %a@;" print_addr_set removed_blocks;
      if c1.ages != c2.ages then begin
            (* this is shallow equals - does it make sense? *)
        Format.fprintf fmt "@;@[<v 0>@[Old ages are %a@]"
          (A.print_delta c2.ages) c1.ages;
            (* print fmt c1; *)
        Format.fprintf fmt "@;@[New ages are %a@]@]"
          (A.print_delta c1.ages) c2.ages;
      end
    | _ -> A.print_delta c2.ages fmt c1.ages
      

  (* let is_var cache addr = A.is_var cache.ages (get_block_addr cache addr) *)

  (* For this domain, we don't care about time *)
  let elapse env d = env
  
end

