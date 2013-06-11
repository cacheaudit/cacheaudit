
open Signatures

open Big_int 

let verbose = ref false
let precise_touch = ref true

type adversay = Blurred | SharedSpace

let adversary = ref Blurred

(* Permutation to apply when touching an element of age a in PLRU *)
(* We assume an ordering correspond to the boolean encoding of the tree from *)
(* leaf to root (0 is the most recent, corresponding to all 0 bits is the path *)

let plru_permut assoc a n = if n=assoc then n else
  let rec f a n =  if a=0 then n 
    else if a land 1 = 1 then if n land 1 = 1 then 2*(f (a/2) (n/2)) else n+1
    else (* a even*) if n land 1 = 1 then n else 2*(f (a/2) (n/2))
  in f a n

module CacheMap = Map.Make(struct type t = int let compare = compare end)
module AddrSet = Set.Make(Int64)
module IntSet = Set.Make(struct type t = int let compare = compare end)

module Make (SV: SIMPLE_VALUE_AD) :
 CACHE_ABSTRACT_DOMAIN = struct
  type t = {
    handled_addrs : AddrSet.t; (** holds addresses handled so far *)
    cache_sets : AddrSet.t CacheMap.t;
    (** holds a set of addreses which fall into a cache set
        as implemented now it may also hold addresses evicted from the cache *)
    ages : SV.t;
    (** for each accessed memory address holds its possible ages *)

    cache_size: int;
    line_size: int; (** same as "data block size" *)
    associativity: int;

    num_sets : int; (** computed from the previous three *)
    strategy : cache_strategy; (*Can be LRU or FIFO so far *)
  }

  let print_addr_set fmt = AddrSet.iter (fun a -> Format.fprintf fmt "%Lx " a)




  (* Count the number of n-permutations of the address set addr_set *)
  let num_tuples (is_valid:var list -> bool) (n: int) (addr_set: AddrSet.t) = 
    if AddrSet.cardinal addr_set >= n then begin
      let rec loop n elements tuple s = 
        if n = 0 then begin
          if is_valid tuple then s+1 else s
        end else
          AddrSet.fold (fun addr s -> 
            loop (n-1) (AddrSet.remove addr elements) (addr::tuple) s) 
            elements s in 
        loop n addr_set [] 0
    end else 0
    
  (* Checks if the given cache state is valid *)
  (* with respect to the ages defined in cache.ages. *)
  let is_valid_cstate (cache:t) (addr_set:AddrSet.t) (cache_state: var list)  = 
    let rec pos addr l i = match l with 
       [] -> cache.associativity 
    | hd::tl -> if hd = addr then i else pos addr tl (i+1) in
    AddrSet.for_all (fun (addr:var) -> 
      List.mem (pos addr cache_state 0)(SV.get_values cache.ages addr)) addr_set 
  
  (* Computes two lists list where each item i is the number of possible *)
  (* cache states of cache set i for a shared-memory *)
  (* and the disjoint-memory (blurred) adversary *)
  let cache_states_per_set (cache:t) =
    CacheMap.fold (fun _ addr_set (nums,bl_nums) ->
      let num_tpls,num_bl =
        let rec loop i (num,num_blurred) =
          if i > cache.associativity then (num,num_blurred)
          else
            let this_num = 
              num_tuples (is_valid_cstate cache addr_set) i addr_set in
            let this_bl = if this_num > 0 then 1 else 0 in
            loop (i+1) ((num + this_num),num_blurred + this_bl)
         in loop 0 (0,0) in
      (num_tpls::nums,num_bl::bl_nums)) cache.cache_sets ([],[])

  let sum l = List.fold_left (fun sol set_sol -> 
      Int64.mul sol (Int64.of_int set_sol)) Int64.one l
  
  let log_sum l = 
    let s = List.fold_left (fun sol set_sol -> 
      log10 (float_of_int set_sol) +. sol) 0.0 l in 
    s /. (log10 2.0)


  let count_cache_states cache = 
    let nums_cstates,bl_nums_cstates = cache_states_per_set cache in
    match !adversary with
    | Blurred -> big_int_of_int64 (sum bl_nums_cstates)
    | SharedSpace -> big_int_of_int64 (sum nums_cstates)

  let print fmt cache =
    Format.fprintf fmt "@[";
    CacheMap.iter (fun i all_elts ->
        if not (AddrSet.is_empty all_elts) then begin
          Format.fprintf fmt "@[ Set %4d: " i;
          AddrSet.iter (fun elt -> Format.fprintf fmt "%Lx @," elt) all_elts;
          Format.fprintf fmt "@]"
        end
      ) cache.cache_sets;
    Format.fprintf fmt "@.Possible ages of blocks:@; %a@]" SV.print cache.ages;
    let nums_cstates,bl_nums_cstates = cache_states_per_set cache in
    if cache.strategy = PLRU then 
      Format.fprintf fmt "Counting on PLRU is incorrect\n";
    Format.printf "\n";
    Format.fprintf fmt "\nNumber of valid cache configurations : 
      0x%Lx, that is %f bits.\n" (sum nums_cstates) (log_sum nums_cstates);
    Format.fprintf fmt "\nNumber of valid cache configurations (blurred): 
      0x%Lx, that is %f bits.\n" (sum bl_nums_cstates) (log_sum bl_nums_cstates)
  let var_to_string x = Printf.sprintf "%Lx" x 
  
  let init (cs,ls,ass,strategy) =
    let ns = cs / ls / ass in
    let rec init_csets csets i = match i with
    | 0 -> csets
    | n -> init_csets (CacheMap.add (n - 1) AddrSet.empty csets) (n - 1) in
    { cache_sets = init_csets CacheMap.empty ns;
      ages = SV.init_with_max var_to_string ass;
      handled_addrs = AddrSet.empty;
      cache_size = cs;
      line_size = ls;
      associativity = ass;
      num_sets = ns;
      strategy = strategy;
    }

  (** Determine the set in which an address is cached
      calculated addr mod num_sets *)
  let get_set_addr cache addr =
    Int64.to_int (Int64.rem addr (Int64.of_int cache.num_sets))

  (** Gives the block address *)
  let get_block_addr cache addr = Int64.div addr (Int64.of_int cache.line_size)

  let get_keys map = let keys,_ = List.split (ValMap.bindings map)
                     in List.map Int64.to_int keys
                    (*TODO simplify this in Simple Values *)

  (** Removes a block when we know it cannot be in the cache *)
  let remove_block cache addr =
    let addr_set = get_set_addr cache addr in
    let cset = CacheMap.find addr_set cache.cache_sets in
    let cset = AddrSet.remove addr cset in
    { cache with
      handled_addrs = AddrSet.remove addr cache.handled_addrs;
      cache_sets = CacheMap.add addr_set cset cache.cache_sets;
    } (*TODO: remove the block from the ages ? *)


  let get_set_diffs aset1 aset2 =
     AddrSet.diff aset1 aset2, AddrSet.diff aset2 aset1

  let join c1 c2 =
    assert ((c1.associativity = c2.associativity) && 
      (c1.num_sets = c2.num_sets));
    let handled_addrs = AddrSet.union c1.handled_addrs c2.handled_addrs in
    let cache_sets = CacheMap.merge 
      (fun k x y ->
        match x,y with
        | Some cset1, Some cset2 ->
           Some (AddrSet.union cset1 cset2)
        | Some cset1, None -> Some cset1
        | None, Some cset2 -> Some cset2
        | None, None -> None 
      ) c1.cache_sets c2.cache_sets in
    let assoc = c1.associativity in
    let haddr_1minus2,haddr_2minus1 =
      get_set_diffs c1.handled_addrs c2.handled_addrs in
    (* add missing variables to ages *)
    let ages1 = AddrSet.fold (fun addr c_ages ->
      SV.set_var c_ages addr assoc) haddr_2minus1 c1.ages in
    let ages2 = AddrSet.fold (fun addr c_ages ->
      SV.set_var c_ages addr assoc) haddr_1minus2 c2.ages in
    let ages = SV.join ages1 ages2 in
    { c1 with ages = ages; handled_addrs = handled_addrs;
    cache_sets = cache_sets}

(* when addr is touched (and already in the cache set)
 update of the age of addr_in *)
(* In case where addr_in can be either older or youner than the intial age *)
(* of addr, splits the cases and returns two cache configurations *)
(* to allow some precision gain *)

  let age_one_element cache addr addr_in =
    if addr = addr_in then cache,None (*This case is treated later in touch*)
    else
      let young,nyoung = SV.comp cache.ages addr_in addr in
      match young with
        Bot -> (match nyoung with
          Bot ->
            (* This case is possible if addr and addr_in have only maximal *)
            (* age (should be out of the cache). TODO: sanity check here ? *)
            remove_block cache addr_in, None
        | Nb nyenv -> { cache with ages = nyenv }, None)
      | Nb yenv ->
         { cache with ages = SV.inc_var yenv addr_in },
           match nyoung with
             | Bot -> None
             | Nb nyenv -> Some {cache with ages = nyenv }

(* Given a cache and a block adress adrr, the first element is the list of blocks in that block's set that can be in the cache *)
  let rec precise_age_elements cache addr = function
    [] -> cache
  | addr_in :: clist -> (match age_one_element cache addr addr_in with
      new_cache, None -> precise_age_elements new_cache addr clist
    | cache1, Some cache2 ->
       let c1 = precise_age_elements cache1 addr clist in
       let c2 = precise_age_elements cache2 addr clist in
(* TODO: see if it is too costly to remove some blocks here, as their*)
(*  could be some of them which need to be put back in the join *)
       join c1 c2)
(*Increments all ages in the set by one *)           
  let incr_ages ages cset = match ages with Bot -> Bot
    | Nb ages -> 
            Nb(AddrSet.fold (fun addr_in a -> SV.inc_var a addr_in) cset ages)
            
            
  let get_ages cache addr = SV.get_values cache.ages (get_block_addr cache addr)

(* returns the set of ages for addr_in that are different from the ages of addr *)
  let ages_different ages addr addr_in =
      let young,nyoung = SV.comp ages addr_in addr in
      lift_combine SV.join young nyoung
(* The effect of one touch of addr, restricting to the case when addr is of age c *)
  let one_plru_touch ages assoc cset addr c = 
    let ages_at_c = SV.exact_val ages addr c in
    if c=assoc then incr_ages ages_at_c cset
    else match ages_at_c with Bot -> Bot
    | Nb ag -> (try 
        Nb(AddrSet.fold (fun addr_in a -> if addr_in=addr then a
                else match ages_different a addr addr_in with
                  Bot -> raise Bottom
                | Nb a -> SV.permute a (plru_permut assoc c) addr_in) cset ag )
      with Bottom -> Bot)

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
      {cache with ages = ages; handled_addrs = h_addrs; cache_sets = cache_sets}

  (** Reads or writes an address into cache *)
  let touch cache orig_addr =
    let env = 

    if !verbose then Printf.printf "\nWriting cache %Lx" orig_addr;
    (* we cache the block address *)
    let addr = get_block_addr cache orig_addr in
    if !verbose then Printf.printf " in block %Lx\n" addr;
    let set_addr = get_set_addr cache addr in
    let cset = CacheMap.find set_addr cache.cache_sets in
    if AddrSet.mem addr cache.handled_addrs then begin
      (* let current_status =                                     *)
      (*   let ages = get_ages cache orig_addr in                 *)
      (*   if ages = [cache.associativity] then M                 *)
      (*   else if not (List.mem cache.associativity ages) then H *)
      (*   else T in                                              *)
      (* let traces = T.add cache.traces current_status in        *)
      let new_cache =
        match cache.strategy with
        | LRU -> 
          let cache = if !precise_touch 
          then precise_age_elements cache addr (AddrSet.elements cset)
          else AddrSet.fold (fun addr_in curr_cache ->
      	    match age_one_element curr_cache addr addr_in with
      	      c, None -> c
      	    | c1, Some c2 -> (* in this case, only the ages differ *)
        		{c1 with ages = SV.join c1.ages c2.ages}
        	  ) cset cache
          in {cache with ages = SV.set_var cache.ages addr 0}
        | FIFO -> (* We first split the cache ages in cases where addr is in the block and cases where it is not *)
          let ages_in, ages_out = 
            SV.comp_with_val cache.ages addr cache.associativity in
          let cache1 = match ages_in with Bot -> Bot
            | Nb ages_in -> Nb {cache with ages=ages_in} (*nothing changes in that case *)
          and cache2 = (*in this case we increment the age of all blocks in the set *)
            match incr_ages ages_out cset with Bot -> Bot
            | Nb ages -> Nb {cache with ages=SV.set_var ages addr 0}
          in (match lift_combine join cache1 cache2 with 
            Bot -> failwith "Unxepected bottom in touch when the strategy is FIFO"
          | Nb c -> c)
        | PLRU -> (* for each possible age of the block, we apply a different permutation *)
          let addr_ages = SV.get_values cache.ages addr in
          let ages = List.fold_left 
            (fun ages addr_age -> 
              lift_combine SV.join ages 
                (one_plru_touch cache.ages cache.associativity cset addr addr_age)
            ) Bot addr_ages in
          (match ages with 
            Bot -> failwith "Unxepected bottom in touch when the strategy is PLRU"
          | Nb ages -> {cache with ages = SV.set_var ages addr 0}
          )
      (* in {new_cache with traces = traces} *)
      in new_cache
    end else add_new_address cache addr set_addr cset 
    in 
      env

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
    assert 
      ((c1.associativity = c2.associativity) && (c1.num_sets = c2.num_sets));
    (AddrSet.subset c1.handled_addrs c2.handled_addrs) &&
    (SV.subseteq c1.ages c2.ages) &&
    (CacheMap.for_all (fun addr vals ->
      if CacheMap.mem addr c2.cache_sets
      then AddrSet.subset vals (CacheMap.find addr c2.cache_sets)
      else false
     ) c1.cache_sets)


  let print_delta c1 fmt c2 =
    Format.fprintf fmt "@[";
    let added_blocks = AddrSet.diff c2.handled_addrs c1.handled_addrs
    and removed_blocks = AddrSet.diff c1.handled_addrs c2.handled_addrs in
    if not (AddrSet.is_empty added_blocks) then Format.fprintf fmt
      "Blocks added to the cache: %a@;" print_addr_set added_blocks;
    if not (AddrSet.is_empty removed_blocks) then Format.fprintf fmt
      "Blocks removed from the cache: %a@;" print_addr_set removed_blocks;
    if c1.ages != c2.ages then begin
      (* this is shallow equals - does it make sense? *)
      Format.fprintf fmt "@[<2> Old ages are %a@]@."
        (SV.print_delta c2.ages) c1.ages;
(*       print fmt c1; *)
      Format.fprintf fmt "@[<2> New ages are %a@]@."
        (SV.print_delta c1.ages) c2.ages;
    end;
    Format.fprintf fmt "@]"

  let is_var cache addr = SV.is_var cache.ages (get_block_addr cache addr)

  (* For this domain, we don't care about time *)
  let elapse env d = env
  
end

                                                                                   *)

