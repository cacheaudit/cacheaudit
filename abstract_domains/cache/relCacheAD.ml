(* TODO: I don't understand why we need a special cacheAD for relational domains? This is bad design...*)

open Signatures
open AD.DataStructures

let verbose = ref false
let precise_touch = ref true

type adversay = Blurred | SharedSpace

let adversary = ref Blurred

module CacheMap = Map.Make(struct type t = int let compare = compare end)
module AddrSet = Set.Make(Int64)
module AddrMap = Map.Make(Int64)
module IntSet = Set.Make(struct type t = int let compare = compare end)


module Make (SV: SimpleRelSetAD.S) : CacheAD.S = struct
  type t = {
    (* holds addresses handled so far *)
    handled_addrs : AddrSet.t;
    (* holds a set of addreses which fall into a cache set *)
    (* as implemented now it may also hold addresses evicted from the cache *)
    cache_sets : AddrSet.t CacheMap.t; 
    (* for each accessed memory address holds its possible ages *)
    ages : SV.t;
    
    cache_size: int;
    line_size: int; (* same as "data block size" *)
    associativity: int;
    num_sets : int; (* computed from the previous three *)
    strategy : cache_strategy;
  }

  (* Determine the set in which an address is cached *)
  (* calculated addr mod num_sets *)
  (* wouldn't work correctly on negative addresses (remainder used and not modulo) *)
  let get_set_addr cache addr =  Int64.to_int (Int64.rem addr (Int64.of_int cache.num_sets))

  let print_addr_set fmt = AddrSet.iter (fun a -> Format.fprintf fmt "%Lx " a)
 
  (* (* Returns a list of all n-tuples that can be created from the adresses in a. *)                                                                            *)
  (* let rec n_tuples (valid:var list -> bool)(n: int) (a: AddrSet.t) : var list list = match n with                                                             *)
  (*    0 -> [[]](*AddrSet.fold (fun _ vll -> []::vll ) a []*)                                                                                                   *)
  (*  | n -> AddrSet.fold (fun addr vll -> let n_minus_one_tuples = n_tuples (fun _ -> true) (n-1) (AddrSet.remove addr a) in                                    *)
  (* List.fold_left (fun vll' x -> let l = addr::x in if valid l then l::vll' else vll') vll n_minus_one_tuples) a []                                            *)

  (* (* Checks if the given cache state is valid with respect to the ages defined in cache.ages. *)                                                              *)
  (* let valid_cache_state (cache:t) (addr_set:AddrSet.t) (cache_state: var list) : bool =                                                                       *)
  (*  let rec pos addr l i = match l with                                                                                                                        *)
  (*      [] -> cache.associativity                                                                                                                              *)
  (*   | hd::tl -> if hd = addr then i else pos addr tl (i+1) in                                                                                                 *)
  (*  AddrSet.for_all (fun (addr:var) -> List.mem (pos addr cache_state 0) (SV.get_values cache.ages addr) ) addr_set                                            *)

  (* (* Helper for cache_states_per_set and blurred_cache_states_per_set *)                                                                                      *)
  (* let rec num_blocks n = match n with 0 -> [0] | m -> m::num_blocks (m-1)                                                                                     *)

  (* (* Computes a list where each item i is the number of possible cache states of cache set i. *)                                                              *)
  (* let cache_states_per_set (cache:t) : int list =                                                                                                             *)
  (*   CacheMap.fold (fun (set_number:int) (addr_set:AddrSet.t) (sol:int list) ->                                                                                *)
  (*     let tuples = List.fold_left (fun l i -> List.append l (n_tuples (valid_cache_state cache addr_set) i addr_set)) [] (num_blocks cache.associativity) in  *)
  (*     let set_solutions = List.length (List.filter (fun (cache_state: var list) -> valid_cache_state cache addr_set cache_state) tuples) in                   *)
  (*   set_solutions::sol) cache.cache_sets []                                                                                                                   *)

  (* (* Same as cache_states_per_set, but the adversary can only see the number of blocks *)                                                                     *)
  (* let blurred_cache_states_per_set (cache:t) : int list =                                                                                                     *)
  (*   CacheMap.fold (fun (set_number:int) (addr_set:AddrSet.t) (sol:int list) ->                                                                                *)
  (*     let tuples = List.fold_left (fun l i -> List.append l (n_tuples (valid_cache_state cache addr_set) i addr_set)) [] (num_blocks cache.associativity) in  *)
  (*     let tmp_solutions = List.filter (fun (cache_state: var list) -> valid_cache_state cache addr_set cache_state) tuples in                                 *)
  (*     let blurred_solutions = List.fold_left (fun (res:IntSet.t) (state:var list) -> IntSet.add (List.length state) res) IntSet.empty tmp_solutions in        *)
  (*     let set_solutions = IntSet.cardinal blurred_solutions in                                                                                                *)
  (*   set_solutions::sol) cache.cache_sets []                                                                                                                   *)

  (* (* Computes the number of possible cache states in a logarithmic scale *)                                                                                   *)
  (* let log_cache_states (cache:t) (counting_fun:t->int list): float = (match cache.strategy with                                                               *)
  (*   PLRU -> Format.printf "Counting on PLRU is incorrect\n"                                                                                                   *)
  (* | _ -> ());                                                                                                                                                 *)
  (*    let sum = List.fold_left (fun sol set_sol -> log10 (float_of_int set_sol) +. sol) 0.0 (counting_fun cache) in                                            *)
  (*    sum /. (log10 2.0)                                                                                                                                       *)

  (* (* Computes the number of possible cache states in an absolute scale *)                                                                                     *)
  (* let absolute_cache_states (cache:t) (counting_fun:t->int list): int64 =                                                                                     *)
  (*    List.fold_left (fun sol set_sol -> Int64.mul sol (Int64.of_int set_sol)) Int64.one (counting_fun cache)                                                  *)

  (*partition helper *)
  let rec add_to_setlist setlist set  =     match setlist with 
      hd::tl -> if IntSet.exists (fun v -> IntSet.mem v hd) set then 
                add_to_setlist tl (IntSet.union set hd) else
                hd::add_to_setlist tl set
    | []     -> [set]

  let merge_sets (unmerged_sets:IntSet.t list) : IntSet.t list  = 
    List.fold_left (fun merged_sets set -> add_to_setlist merged_sets set) [] unmerged_sets
  

type af = (var*int) list

(* replace the element at position n in the list l with element e *)
  let rec list_replace (n:int) (l: 'a list) (e: 'a) : 'a list = 
    match n with 0 -> e :: (List.tl l) | m -> List.hd l :: list_replace (n-1) (List.tl l) e

  let satisfies_cache_constraints (cache:t) (state:af) (inv_cachemap) : bool = 
    (* Split list s.t. we get 1 list per cache set*)
    let split_state : (int * af) list = 
       let rec insert ((v,i):var * int) (l:(int * af) list) = 
         if i = cache.associativity then l else (* dont insert blocks which are not in the cache *)
         match l with 
           [] -> [(AddrMap.find v inv_cachemap ,[(v,i)])] 
         |(setnr,l')::tl -> if AddrMap.find v inv_cachemap = setnr then 
                            (setnr,(v,i)::l')::tl else
                            (setnr,l')::insert (v,i) tl
       in
       List.fold_left (fun result (v,i) -> insert (v,i) result) [] state in
     (* Check constraints for each set *)
    let valid (state:af) :bool = 
       (* Compute occurrences of positions *)
       let rec helper n : int list = match n with 0 -> [] | x -> 0::helper (x-1) in
       let occurrences = List.fold_left (fun occ (v,i)-> list_replace i occ ((List.nth occ i)+1)) (helper cache.associativity) (state) in
       (* Check if every position in the cache occurs only once and has a predecessor *)
       let rec check (l:int list) : bool = match l with
           [] -> true
         | hd::tl -> if hd = 0 then List.for_all (fun i -> i = 0) tl else (hd = 1) && check tl in 
       check occurrences in 
    List.for_all (fun (setnr,af) -> valid af) split_state

  let valid_state (cache:t) (state:(var * int) list): bool = 
    SV.mem cache.ages state

  let cache_states_of_partition (cache:t) (setnums:IntSet.t) : int = 
    (* invert cache map *)
    let inv_cachemap = CacheMap.fold (fun setnum addrset map -> AddrSet.fold (fun addr map'-> AddrMap.add addr setnum map')addrset map) cache.cache_sets AddrMap.empty in
   (* function to compute the solutions of this partition *)
    let rec solutions (v:var) (values:int list) (unused: var list) (current:(var * int) list) : int = 
      match unused with 
         [] -> List.fold_left (fun result i -> if valid_state cache ((v,i)::current) && satisfies_cache_constraints cache ((v,i)::current) inv_cachemap(* *) then 1+result else result) 0 values
      | hd::tl -> let new_values = SV.get_values cache.ages hd in
         List.fold_left (fun result i -> if valid_state cache ((v,i)::current)(* *) then result + solutions hd new_values tl ((v,i)::current) else result) 0 values
       in
    let all_addresses = CacheMap.fold (fun num set result -> if IntSet.mem num setnums then AddrSet.union set result else result ) cache.cache_sets  AddrSet.empty in
    match AddrSet.elements all_addresses with [] -> 0 | hd::tl -> solutions hd (SV.get_values cache.ages hd) tl []

  let cache_states_per_partition (cache:t) (setnums:IntSet.t list) : int list = 
    List.fold_left (fun l setnums -> cache_states_of_partition cache setnums::l) [] setnums
(* Combine agelists *)

  let comp_setnums (cache:t) : IntSet.t list = let fmt = Format.std_formatter in
    (* Compute partitions induced by constraints *)
    let partitions = List.map (fun vlist -> List.fold_left (fun result elem -> AddrSet.add elem result) AddrSet.empty vlist) (SV.partition cache.ages) in
    (* convert variables to their respective set number *)
    let setnums = List.fold_left (fun newlist set -> (AddrSet.fold (fun addr intset -> IntSet.add (get_set_addr cache addr) intset) set IntSet.empty)::newlist) [] partitions in
    (* merge sets containing the same set numbers *)
    let result = merge_sets setnums in
    Format.fprintf fmt "Partitioning leads to parts of size: (";List.iter (fun set -> Format.fprintf fmt " %d, " (IntSet.cardinal set)) result; Format.fprintf fmt ") cache sets\n";Format.print_flush ();
    result

  let rel_absolute_cache_states (cache:t) (states_per_part:int list): int64 = 
    (* Multiply numbers *)
    List.fold_left (fun sol set_sol -> Int64.mul sol (Int64.of_int set_sol)) Int64.one states_per_part

  let rel_log_cache_states (cache:t) (states_per_part:int list) : float = 
     let sum = List.fold_left (fun sol set_sol -> log10 (float_of_int set_sol) +. sol) 0.0 states_per_part in
     sum /. (log10 2.0)

  let rel_cache_states (cache:t) : int64 * float = 
    let setnums = comp_setnums cache in
    let states_per_part = cache_states_per_partition cache setnums in
    (rel_absolute_cache_states cache states_per_part, rel_log_cache_states cache states_per_part)

open Big_int

  let count_cache_states cache = failwith "count_cache_states not implemented for relational cache"
(*     match !adversary with                                                                                                                         *)
(* (* TODO: compute a better bound when we have a bulrred adversary *)                                                                               *)
(*        Blurred -> List.fold_left (fun sol set_sol -> mult_big_int sol (big_int_of_int set_sol)) unit_big_int (blurred_cache_states_per_set cache) *)
(*     | SharedSpace ->                                                                                                                              *)
(*         let setnums = comp_setnums cache in                                                                                                       *)
(*         let states_per_part = cache_states_per_partition cache setnums in                                                                         *)
(*         List.fold_left (fun sol set_sol -> mult_big_int sol (big_int_of_int set_sol)) unit_big_int states_per_part                                *)

  let print fmt cache =
    Format.fprintf fmt "@[";
    CacheMap.iter (fun i all_elts -> 
        if not(AddrSet.is_empty all_elts) then (
          Format.fprintf fmt "@[ Set %4d: " i;
          AddrSet.iter (fun elt -> Format.fprintf fmt "%Lx @," elt) all_elts;
          Format.fprintf fmt "@]"
        )
      ) cache.cache_sets;
     Format.fprintf fmt "@.Possible ages of blocks:@; %a@]" SV.print cache.ages;
(* Format.fprintf fmt "\nNumber of valid cache configurations : 0x%Lx, that is %f bits.\n" (absolute_cache_states cache cache_states_per_set) (log_cache_states cache cache_states_per_set);                          *)
(* Format.fprintf fmt "\nNumber of valid cache configurations (blurred): 0x%Lx, that is %f bits.\n" (absolute_cache_states cache blurred_cache_states_per_set) (log_cache_states cache blurred_cache_states_per_set); *)
let (rel_abs,rel_log) = rel_cache_states cache in Format.fprintf fmt "Valid cache configurations computed with relational Information : 0x%Lx, that is %f bits.\n" rel_abs rel_log
 
  let var_to_string x = Printf.sprintf "%Lx" x 
  
  let init (cs,ls,ass,strategy) =
    let ns = cs / ls / ass in
    let rec init_csets csets i = match i with
      | 0 -> csets
      | n -> init_csets (CacheMap.add (n-1) AddrSet.empty csets) (n-1) in
    { cache_sets = init_csets CacheMap.empty ns; 
      ages = SV.init_with_max var_to_string ass; 
      handled_addrs = AddrSet.empty; 
      cache_size = cs;
      line_size = ls;
      associativity = ass;
      num_sets = ns;
      strategy = strategy;
    }

    
	(* Gives the block address *)
  let get_block_addr cache addr = Int64.div addr (Int64.of_int cache.line_size)
  
  let get_keys map = let keys,_ = List.split (ValMap.bindings map)
                     in List.map Int64.to_int keys (*TODO simplify this in Simple Values *)
 
  (* Removes a block when we know it cannot be in the cache *)
  let remove_block cache addr = 
    let addr_set = get_set_addr cache addr in
    let cset = CacheMap.find addr_set cache.cache_sets in
    let cset = AddrSet.remove addr cset in
    {cache with
      handled_addrs = AddrSet.remove addr cache.handled_addrs;
      cache_sets = CacheMap.add addr_set cset cache.cache_sets;
    } (*TODO: remove the block from the ages ? *)

  
  let get_set_diffs aset1 aset2 = 
     AddrSet.diff aset1 aset2, AddrSet.diff aset2 aset1
  
  let join c1 c2 = 
(*     if !verbose then Printf.printf "before join\n"; *)
    assert ((c1.associativity = c2.associativity) && (c1.num_sets = c2.num_sets));
    let handled_addrs = AddrSet.union c1.handled_addrs c2.handled_addrs in
    let cache_sets = CacheMap.merge (fun k x y -> match x,y with 
                                     | Some cset1, Some cset2 -> Some (AddrSet.union cset1 cset2)
                                     | Some cset1, None -> Some cset1
                                     | None, Some cset2 -> Some cset2
                                     | None, None -> None
                                    ) c1.cache_sets c2.cache_sets in
    let assoc = c1.associativity in
    let haddr_1minus2,haddr_2minus1 = get_set_diffs c1.handled_addrs c2.handled_addrs in
    (* add missing variables to ages *)
    let ages1 = AddrSet.fold (fun addr c_ages -> SV.set_var c_ages addr assoc) haddr_2minus1 c1.ages in
    let ages2 = AddrSet.fold (fun addr c_ages -> SV.set_var c_ages addr assoc) haddr_1minus2 c2.ages in
    let ages = SV.join ages1 ages2 in
    {c1 with ages = ages; handled_addrs = handled_addrs; cache_sets = cache_sets;}
    
(* when addr is touched (and already in the cache set) update of the age of addr_in *)
(* In case where addr_in can be either older or youner than the intial age of addr, splits the cases and returns two cache configurations to allow some precision gain *)
  let age_one_element cache addr addr_in =
    if addr = addr_in then cache,None
    else
      let young,nyoung = SV.comp cache.ages addr_in addr in
      match young with
        Bot -> (match nyoung with
          Bot -> (* This case is possible if addr and addr_in have only maximal age (should be out of the cache). TODO: sanity check here ? *)
            remove_block cache addr_in, None
        | Nb nyenv -> {cache with ages = nyenv}, None)
      | Nb yenv ->
         {cache with ages = SV.inc_var yenv addr_in},
         match nyoung with
           | Bot -> None
           | Nb nyenv -> Some {cache with ages = nyenv }

  let rec precise_age_elements cache addr = function
    [] -> cache
  | addr_in::clist -> (match age_one_element cache addr addr_in with
      new_cache, None -> precise_age_elements new_cache addr clist
    | cache1, Some cache2 -> 
       let c1 = precise_age_elements cache1 addr clist in
       let c2 = precise_age_elements cache2 addr clist in
(* TODO: see if it is too costly to remove some blocks here, as their could be some of them which need to be put back in the join *)
       join c1 c2)
            

   
  let get_ages cache addr = SV.get_values cache.ages (get_block_addr cache addr)
  
  (* adds a new address handled by the cache if it's not already handled *)
   (* We increment the age of all other adresses in the same set *)
   (* That works for LRU, FIFO and PLRU *)
   let add_new_address cache addr set_addr cset =
      let ages = SV.set_var cache.ages addr 0 in
      let h_addrs = AddrSet.add addr cache.handled_addrs in
      (* increment the ages of elements in cache set *)
      (* also ages >= associativity are incremented; we may not want this *)
      let ages = AddrSet.fold (fun addr oages -> SV.inc_var oages addr) cset ages in
      let cache_sets = CacheMap.add set_addr (AddrSet.add addr cset) cache.cache_sets in
      {cache with ages = ages; handled_addrs = h_addrs; cache_sets = cache_sets;}

  
  (* touch: read or write cache at address addr *)
  let touch cache orig_addr = 
    if !verbose then Printf.printf "\nWriting cache %Lx" orig_addr;
    let addr = get_block_addr cache orig_addr in (* we cache the block address *)
    if !verbose then Printf.printf " in block %Lx\n" addr;
    let set_addr = get_set_addr cache addr in
    let cset = CacheMap.find set_addr cache.cache_sets in
    if AddrSet.mem addr cache.handled_addrs then begin
      match cache.strategy with
        LRU -> let cache = if !precise_touch
          then precise_age_elements cache addr (AddrSet.elements cset)
          else AddrSet.fold (fun addr_in curr_cache ->
            match age_one_element curr_cache addr addr_in with
              c, None -> c
            | c1, Some c2 -> (* in this case, only the ages differ *)
                {c1 with ages = SV.join c1.ages c2.ages}
                          ) cset cache
        in {cache with ages = SV.set_var cache.ages addr 0}
      | FIFO -> (* We first split the cache ages in cases where addr is in the blocck and cases where it is not *)
        let ages_in, ages_out =
          SV.comp_with_val cache.ages addr cache.associativity in
        let cache1 = match ages_in with Bot -> Bot
          | Nb ages_in -> Nb {cache with ages=ages_in} (*nothing changes in that case *)
        and cache2 = (*in this case we increment the age of all blocks in the set *)
          match ages_out with Bot -> Bot
          | Nb ages_out ->
              let ages = AddrSet.fold (fun addr_in a -> SV.inc_var a addr_in)
                                  cset ages_out
              in Nb {cache with ages=SV.set_var ages addr 0}
        in (match lift_combine join cache1 cache2 with
          Bot -> failwith "Unxepected bottom in touch when the strategy is FIFO"
        | Nb c -> c)
      | PLRU -> failwith "Pseudo LRU cache strategy not analyzed yet\n"
    end else begin (* this works for FIFO, PLRU and LRU *)
      let ages = SV.set_var cache.ages addr 0 in
      let h_addrs = AddrSet.add addr cache.handled_addrs in
      (* increment the ages of elements in cache set *)
      (* also ages >= associativity are incremented; we may not want this *)
      let ages = AddrSet.fold (fun addr oages -> SV.inc_var oages addr) cset ages in
      let cache_sets = CacheMap.add set_addr (AddrSet.add addr cset) cache.cache_sets in
      {cache with ages = ages; handled_addrs = h_addrs; cache_sets = cache_sets;}
    end

  (* Same as touch, but returns two possible configurations, one for the hit and the second for the misses *)
  (* TODO: we could be more efficient here, by not calling touch, but modifying touch instead *)
  let touch_hm cache orig_addr =
    let addr = get_block_addr cache orig_addr in
    let set_addr = get_set_addr cache addr in
    let cset = CacheMap.find set_addr cache.cache_sets in
    if AddrSet.mem addr cache.handled_addrs then begin
      (* ages_in is the set of ages for which there is a hit *)
      let ages_in, ages_out =
          SV.comp_with_val cache.ages addr cache.associativity in
      let t a = match a with Bot -> Bot
                | Nb a -> Nb(touch {cache with ages=a} orig_addr)
      in
      (t ages_in, t ages_out)
    end else (Bot, Nb(add_new_address cache addr set_addr cset))


  let widen c1 c2 = 
    join c1 c2


  let subseteq c1 c2 =
    assert ((c1.associativity = c2.associativity) && (c1.num_sets = c2.num_sets));
    (AddrSet.subset c1.handled_addrs c2.handled_addrs) && (SV.subseteq c1.ages c2.ages) &&
    (CacheMap.for_all (fun addr vals -> if CacheMap.mem addr c2.cache_sets then
                                          AddrSet.subset vals (CacheMap.find addr c2.cache_sets)
                                        else false
                      ) c1.cache_sets)


  let print_delta c1 fmt c2 = 
    Format.fprintf fmt "@[";
    let added_blocks = AddrSet.diff c2.handled_addrs c1.handled_addrs
    and removed_blocks = AddrSet.diff c1.handled_addrs c2.handled_addrs in
    if not(AddrSet.is_empty added_blocks) then Format.fprintf fmt "Blocks added to the cache: %a@;" print_addr_set added_blocks;
    if not(AddrSet.is_empty removed_blocks) then Format.fprintf fmt "Blocks removed from the cache: %a@;" print_addr_set removed_blocks;
    if c1.ages != c2.ages then begin (* this is shallow equals - does it make sense? *)
      Format.fprintf fmt "@[<2> Old ages are %a@]@." (SV.print_delta c2.ages) c1.ages;
(*       print fmt c1; *)
      Format.fprintf fmt "@[<2> New ages are %a@]@." (SV.print_delta c1.ages) c2.ages;
    end;
    Format.fprintf fmt "@]"
    
    
 
  let is_var cache addr = SV.is_var cache.ages (get_block_addr cache addr)

  (* For this domain, we don't care about time *)
  let elapse env d = env
  
end 

(* module RelSetCacheAD = RelCacheAD (SimpleRelSetAD.SimpleRelSetAD) *)
