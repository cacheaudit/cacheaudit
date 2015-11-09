(* Copyright (c) 2013-2015, IMDEA Software Institute.             *)
(* See ../../LICENSE for authorship and licensing information     *)
open Big_int
  
open AD.DS
  
open Logger
  
open NumAD.DS
  
type cache_st = | H | M | N

(* Hit, Miss, No access, Hit or Miss *)
let cachest_to_int64 = function | H -> 1L | M -> 2L | N -> 3L
  
let int64_to_cachest =
  function | 1L -> H | 2L -> M | 3L -> N | _ -> assert false
  
let (duration_H, duration_M, duration_N) = (3, 20, 1)
  
let max_times = 10000000
  
let delta = 4
    
module Make (CA : CacheAD.S) =
  struct
    type 'a parent_t = | Root | Single of 'a | Couple of 'a * 'a
    
    module rec Trie :
                 sig
                   type 'a t =
                     { parents : ('a t) parent_t; parent_UIDs : NumSet.t;
                       node_UID : Int64.t; value : 'a; num_traces : big_int
                     }
                   
                   val compare : 'a t -> 'a t -> int
                     
                 end =
                 struct
                   type 'a t =
                     { parents : ('a t) parent_t; parent_UIDs : NumSet.t;
                       node_UID : Int64.t; value : 'a; num_traces : big_int
                     }
                   
                   let compare n1 n2 =
                     Pervasives.compare ((n1.value), (n1.parent_UIDs))
                       ((n2.value), (n2.parent_UIDs))
                     
                 end
      
    type wblocks_t =
      { node1 : NumSet.t Trie.t;
        (* stores the trace; 
      multiple repetitions are stored as one access *)
        node2 : (NumSet.t Trie.t) option;
        (* may store a second trace in case we want 
      to delay the join *)
        reps : IntSet.t NumMap.t
      }
    
    (* stores number of occurrences of elements *)
    type t =
      { traces : (NumSet.t Trie.t) add_top; cache : CA.t add_top;
        times : IntSet.t add_top; addr_trace : (NumSet.t Trie.t) add_top;
        block_trace : (NumSet.t Trie.t) add_top; 
        wblocks : wblocks_t add_top;
        cache_param : CacheAD.cache_param_t
      }
    
    let is_dummy n = NumSet.is_empty n.Trie.value
      
    let get_parent_UIDs =
      function
      | Root -> NumSet.empty
      | Single p -> NumSet.singleton p.Trie.node_UID
      | Couple (p1, p2) ->
          (* In case that a parent p1 is dummy and p2 not, *)
          (* parent_uids = {p2} union (parent_UIDs of p1) *)
          if (is_dummy p1) || (is_dummy p2) then
            if (is_dummy p1) <> (is_dummy p2) then
             (let (p1, p2) = if is_dummy p2 then (p2, p1) else (p1, p2)
              in NumSet.add p2.Trie.node_UID p1.Trie.parent_UIDs)
            else (* both parents dummy *)
              NumSet.union p2.Trie.parent_UIDs p1.Trie.parent_UIDs
          else
             NumSet.of_list [ p1.Trie.node_UID; p2.Trie.node_UID ]
    
    
    (* get the number of traces finishing on the parents *)
    let get_parent_num_traces count_fn parents =
      let count p = 
        let n = p.Trie.num_traces in
        if is_dummy p then n
        else mult_big_int (count_fn p.Trie.node_UID) n in
      match parents with
      | Root -> unit_big_int
      | Single p -> (count p)
      | Couple (p1, p2) -> add_big_int (count p1) (count p2)
      
    let uid = ref 0L
      
    (* Calculates a hash given the current value of a node *)
    (* and the unique IDs of the parents*)
    let hash_node_parents value parent_UIDs =
      Hashtbl.hash (value, parent_UIDs)
      
    module TrieHashtbl =
      Hashtbl.Make
        (struct
           type t = NumSet.t Trie.t
           
           let hash node =
             hash_node_parents node.Trie.value node.Trie.parent_UIDs
             
           let equal n1 n2 = (Trie.compare n1 n2) = 0
             
         end)
      
    (* A hash table holding all nodes exactly once *)
    let hitmiss_hashtbl = TrieHashtbl.create 500
      
    let addr_hashtbl = TrieHashtbl.create 500
      
    let block_hashtbl = TrieHashtbl.create 500
      
    let wblock_hashtbl = TrieHashtbl.create 500
      
    (* Find the value in the hash table or add it; return the node *)
    let find_or_add hashtbl node =
      try TrieHashtbl.find hashtbl node
      with | Not_found -> (TrieHashtbl.add hashtbl node node; node)
      
    (* all elements repeated less than delta times are counted as 1 possibility; *)
    (* more repetitions are counted as one possibility each *)
    let count_wbl reps uid = 
      let nums = NumMap.find uid reps in
      (* Printf.printf "(min: %d, max: %d)\n" (IntSet.min_elt nums) (IntSet.max_elt nums); *)
      big_int_of_int (if (IntSet.max_elt nums) - (IntSet.min_elt nums) <= delta 
        then 1 else IntSet.cardinal nums) 
        
      
    
    let count_normal uid = unit_big_int
    
    let create_node count_fn hashtbl parents value =
      (uid := Int64.succ !uid; 
       let num_tr =
         mult_int_big_int (let n = NumSet.cardinal value in if n = 0 then 1 else n)
           (get_parent_num_traces count_fn parents) in
       let parent_UIDs = get_parent_UIDs parents in
       let newnode =
         {
           Trie.parents = parents;
           Trie.parent_UIDs = parent_UIDs;
           Trie.value = value;
           Trie.node_UID = !uid;
           Trie.num_traces = num_tr;
         }
       in find_or_add hashtbl newnode)
      
    let init_cache = ref Tp
      
    let init cache_param =
      (init_cache := Nt (CA.init cache_param);
       {
         traces = Nt (create_node count_normal hitmiss_hashtbl Root NumSet.empty);
         cache = !init_cache;
         times = Nt (IntSet.singleton 0);
         addr_trace = Nt (create_node count_normal addr_hashtbl Root NumSet.empty);
         block_trace = Nt (create_node count_normal block_hashtbl Root NumSet.empty);
         wblocks =
           (let node = create_node count_normal wblock_hashtbl Root NumSet.empty
            in
              Nt {
                    node1 = node;
                    node2 = None;
                    reps =
                      NumMap.singleton node.Trie.node_UID (IntSet.singleton 1);
                });
         cache_param = cache_param;
       })
      
    (* Update node's value*)
    let update_value count_fn hashtbl node value =
      if value = node.Trie.value
      then node
      else create_node count_fn hashtbl node.Trie.parents value
      
    (* Add a new child to a node *)
    let add count_fn hashtbl trace value =
      match trace with
      | Tp -> Tp
      | Nt node ->
          let new_node = create_node count_fn hashtbl (Single node) value
          in Nt new_node
      
    let add_dummy hashtbl parents = create_node count_normal hashtbl parents NumSet.empty
      
    let join_traces count_fn node1 node2 = (* Same trie *)
      if node1.Trie.node_UID = node2.Trie.node_UID
      then Nt node1
      else (* Same parents *)
        if node1.Trie.parent_UIDs = node2.Trie.parent_UIDs
        then
          (* assertion: if parents and values were equal, should have same UID *)
          (assert (node1.Trie.value <> node2.Trie.value);
           (* if node1.Trie.value <> (Some N) && node2.Trie.value <> (Some N) then begin *)
           Nt
             (update_value count_fn hitmiss_hashtbl node1
                (NumSet.union node1.Trie.value node2.Trie.value)))
        else
          (* end else failwith "TraceAD: Joining 'N' not implemented" end *)
          (let parents = Couple (node1, node2)
           in
             (* A dummy node whose parents are the nodes we are joining *)
             Nt (add_dummy hitmiss_hashtbl parents))
    
    (* Handle Top in a join-like fashion: if any of the two states [s1], [s2] *)
    (* is Top, go to Top, otherwise apply [join_fn] *)
    let join_top join_fn s1 s2 =
      match (s1, s2) with 
      | (Nt x, Nt y) -> join_fn x y 
      | (_, _) -> Tp

    let join_none x1 x2 join_fn =
      match x1, x2 with
      | None, x2 -> x2
      | x1, None -> x1
      | Some x1, Some x2 -> Some (join_fn x1 x2)

    let join_none_top x1 x2 join_fn =
      match x1, x2 with
      | None, Some x2 -> Nt x2
      | Some x1, None -> Nt x1
      | Some x1, Some x2 -> join_fn x1 x2
      | None, None -> assert false
    
    let join_reps reps1 reps2 = 
      NumMap.merge (fun _ x1 x2 -> join_none x1 x2 (fun s1 s2 -> 
        (IntSet.union s1 s2))) reps1 reps2
    
    (* adds a new node to reps, with initial count of 1 *)
    let add_to_reps x reps = match x with 
    | Tp -> reps
    | Nt node -> NumMap.add node.Trie.node_UID (IntSet.singleton 1) reps
    
    let join_wblocks wblocks1 wblocks2 =
      let u1, v1, u2, v2 = wblocks1.node1, wblocks1.node2, wblocks2.node1, wblocks2.node2 in
      let uids = NumSet.add u1.Trie.node_UID (NumSet.singleton u2.Trie.node_UID) in
      let add_if_some x xset = 
        match x with Some v -> NumSet.add v.Trie.node_UID xset | None -> xset in
      let uids = add_if_some v1 (add_if_some v2 uids) in 
      let reps = join_reps wblocks1.reps wblocks2.reps in
      if NumSet.cardinal uids > 2 then 
        let n1 = join_none_top (Some u1) v1 (join_traces (count_wbl reps)) in
        let n2 = join_none_top (Some u2) v2 (join_traces (count_wbl reps)) in
        let reps = add_to_reps n1 (add_to_reps n2 reps) in
        let newnode = join_top (join_traces (count_wbl reps)) n1 n2 in
        let reps = add_to_reps newnode reps in
        (* let node = join_none_top (Some u1) v1 (join_traces (count_wbl reps)) in *)
        (* let reps = add_to_reps node reps in                                     *)
        (* let node = join_top (join_traces (count_wbl reps)) node (Nt u2) in      *)
        (* let reps = add_to_reps node reps in                                     *)
        (* let newnode = match node with Tp -> Tp                                  *)
        (* | Nt n -> join_none_top (Some n) v2 (join_traces (count_wbl reps)) in   *)
        (* let reps = add_to_reps newnode reps in                                  *)
        match newnode with 
        | Tp -> Tp
        | Nt n -> Nt { node1 = n; node2 = None; reps = reps }
      else
        (* If there are 2 traces, we postpone the final join until next update *)
        (* (leaving node2 to be not None). With 1 trace, node2 = None *)
        (* This way we may gain precision. *)
        let u1_in, v1_in = if u1.Trie.node_UID = u2.Trie.node_UID then Some u2, v2 
          else v2, Some u2 in
          let join_same_wnode n1 n2 = 
            join_none n1 n2 (fun node1 node2 -> 
              assert (node1.Trie.node_UID = node1.Trie.node_UID);
              node1) in
          let node1 =  match join_same_wnode (Some u1) u1_in with
            | Some n -> n | _ -> assert false in
            Nt { node1 = node1; node2 = join_same_wnode v1 v1_in; 
              reps = reps }
 
    let join_times times1 times2 =
      let tms = IntSet.union times1 times2
      in if (IntSet.cardinal tms) < max_times then Nt tms else Tp
      
    let add_nt fn s1 s2 = Nt (fn s1 s2)
    
    let print_top fmt print_fn top_string state =
      match state with
      | Tp -> Format.fprintf fmt top_string
      | Nt x -> print_fn fmt x
    let print_traces count_fn fmt node =
         let num = mult_big_int 
           (count_fn node.Trie.node_UID) node.Trie.num_traces in
         Format.fprintf fmt "%s, %f bits\n"
           (string_of_big_int num)
           (Utils.log2 num)
    
    let join env1 env2 =
      (* let print_set s =                                  *)
      (*   Printf.printf "value { ";                        *)
      (*   NumSet.iter (fun x -> Printf.printf "%Lx " x) s; *)
      (*   Printf.printf "}\n" in                           *)
      (* let print_current env =                            *)
      (*   match env.addr_trace, env.block_trace with       *)
      (*   | Nt addrs, Nt blks ->                           *)
      (*     print_set addrs.Trie.value;                    *)
      (*     print_set blks.Trie.value                      *)
      (*   | _, _ -> () in                                  *)
      (* print_current env1; print_current env2;            *)
      { 
        env1 with
        traces = join_top (join_traces count_normal) env1.traces env2.traces;
        cache = join_top (add_nt CA.join) env1.cache env2.cache;
        times = join_top join_times env1.times env2.times;
        addr_trace = join_top (join_traces count_normal) env1.addr_trace env2.addr_trace;
        block_trace = join_top (join_traces count_normal) env1.block_trace env2.block_trace;
        wblocks = join_top join_wblocks env1.wblocks env2.wblocks
      }
      
    let widen env1 env2 =
      let cache = join_top (add_nt CA.widen) env1.cache env2.cache in
      (* join_times goes to top at some point *)
      let times = join_top join_times env1.times env2.times in
      let traces =
        match ((env1.traces), (env2.traces)) with
        | (Nt node1, Nt node2) ->
            if node1.Trie.node_UID = node2.Trie.node_UID
            then Nt node1
            else Tp
        | (_, _) -> Tp in
      let addr_trace =
        join_top (join_traces count_normal) env1.addr_trace env2.addr_trace in
      let block_trace =
        join_top (join_traces count_normal) env1.block_trace env2.block_trace in
      let wblocks = failwith "widen: todo"
      in
        {
          (env1)
          with
          cache = cache;
          traces = traces;
          times = times;
          addr_trace = addr_trace;
          block_trace = block_trace;
          wblocks = wblocks;
        }
      
    let rec subseteq_traces node1 node2 =
      if node1.Trie.node_UID = node2.Trie.node_UID
      then true
      else
        if
          (NumSet.subset node1.Trie.value node2.Trie.value) &&

            (* empty set denotes dummy nodes, not a subset of non-dummy nodes *)
            (not
               ((node1.Trie.value = NumSet.empty) &&
                  (node2.Trie.value <> NumSet.empty)))
        then
          (* if (node1.Trie.value = node2.Trie.value) ||                   *)
          (* (node2.Trie.value = Some HM && node1.Trie.value <> None) then *)
          (match ((node1.Trie.parents), (node2.Trie.parents)) with
           | (Root, Root) -> true
           | (Single p1, Single p2) -> subseteq_traces p1 p2
           | (Couple (p11, p12), Couple (p21, p22)) ->
               (subseteq_traces p11 p21) && (subseteq_traces p12 p22)
           | (_, _) -> false)
        else false
      
    let subeq_top subseq_fn s1 s2 =
      match (s1, s2) with
      | (Nt x, Nt y) -> subseq_fn x y
      | (_, Tp) -> true
      | (_, _) -> false
      
    let subseteq_wblocks wbl1 wbl2 = failwith "subseteq_wblocks: todo"
      
    let subseteq env1 env2 =
      (env1.cache_param = env2.cache_param) &&
      ((subeq_top CA.subseteq env1.cache env2.cache) &&
      ((subeq_top IntSet.subset env1.times env2.times) &&
      ((subeq_top subseteq_traces env1.traces env2.traces) &&
      ((subeq_top subseteq_traces env1.addr_trace env2.addr_trace) &&
      ((subeq_top subseteq_traces env1.block_trace env2.block_trace) &&
      (subeq_top subseteq_wblocks env1.wblocks env2.wblocks))))))
       
    let print fmt env =
       Format.fprintf fmt "Not printing final cache state.\n";
       (* Format.fprintf fmt "Final cache state:\n"; *)
       (* print_top fmt CA.print "Top" env.cache;    *)
       Format.fprintf fmt "\n# traces: ";
         (print_top fmt (print_traces count_normal) "too imprecise to tell" env.traces;
          Format.fprintf fmt "\n# times: ";
          let print_times fmt tms =
            let numtimes = IntSet.cardinal tms
            in
              Format.fprintf fmt "%d, %f bits\n" numtimes
                (Utils.log2 (big_int_of_int numtimes))
          in
          ( print_top fmt print_times "too imprecise to tell" env.times;
            Format.fprintf fmt "\naddress-traces: ";
            print_top fmt (print_traces count_normal) "too imprecise to tell"
              env.addr_trace;
            Format.fprintf fmt "\nblock-traces: ";
            print_top fmt (print_traces count_normal) "too imprecise to tell"
              env.block_trace;
            Format.fprintf fmt "\nwblock-traces: ";
            let node, count_fn = match env.wblocks with Tp -> Tp, count_normal
            | Nt wbl -> begin match wbl.node2 with
              | None -> Nt wbl.node1, (count_wbl wbl.reps)
              | Some node2 -> 
                join_traces (count_wbl wbl.reps) wbl.node1 node2, (count_wbl wbl.reps) end in  
             print_top fmt (print_traces count_fn) "too imprecise to tell" node
            ))
            
    let print_delta env1 fmt env2 =
      (* TODO: implement printing of delta of traces and times *)
      match ((env1.cache), (env2.cache)) with
      | (Nt c1, Nt c2) -> CA.print_delta c1 fmt c2
      | (_, _) -> Format.fprintf fmt "\nCache environment: Top.\n"
      
    let add_time time times =
      match times with
      | Tp -> Tp
      | Nt tms ->
          Nt
            (IntSet.fold (fun x tms -> IntSet.add (x + time) tms) tms IntSet.
               empty)
      
    let add_time_status status times =
      match status with
      | H -> add_time duration_H times
      | M -> add_time duration_M times
      | N -> add_time duration_N times
      
    (* returns a unique identifier of the address, assuming the address is *)
    (* either symbolic (like something on the heap), *)
    (* or relative (like something on the stack with unknown offset) *)
    let get_block_id env a =
      let num_line_bits =
         int_of_float
           (ceil (Utils.log2 (big_int_of_int env.cache_param.CacheAD.ls))) in
      (* if not (is_symb a) *)
      (* then a *)
      (* else  *)
        (* two symbolic addresses will be considered equal if *)
        (*  - the symbolic uid is the same *)
        (*  - the same bit indices are known *)
        (*  - the least-significant bits are the same *)
         (* change_bits a 0 num_line_bits 0L *)
      change_bits a 0 num_line_bits 0L
    
    let update_wblocks wblocks block =
      match wblocks with
      | Tp -> Tp
      | Nt wblocks ->
        let increment_reps reps uid = 
          let nums = (NumMap.find uid reps) in
          NumMap.add uid (IntSet.fold (fun n nms ->
            IntSet.add (n + 1) nms) nums IntSet.empty) reps in
        if wblocks.node2 = None then
          let value = wblocks.node1.Trie.value in
          if (NumSet.cardinal value = 1) && (NumSet.choose value) = block then begin 
            (* we are accessing the same block once again *)
            (* => increment number of repetitions *)
            Nt { wblocks with reps = increment_reps wblocks.reps wblocks.node1.Trie.node_UID }
          end else begin
            match add (count_wbl wblocks.reps) wblock_hashtbl (Nt wblocks.node1) (NumSet.singleton block) with
            | Tp -> Tp
            | Nt node1 ->
              (* standard addition of a new node; the count is set to 1 *)
              Nt { node1 = node1;
                node2 = None;
                reps = NumMap.add node1.Trie.node_UID (IntSet.singleton 1) wblocks.reps}
        end else 
          (* Here we have two possible traces for which we delayed the join. *)
          (* We will update both and join them *)
          let node2 = match wblocks.node2 with 
          | None -> assert false
          | Some n -> n in
          match add (count_wbl wblocks.reps) wblock_hashtbl (Nt wblocks.node1) (NumSet.singleton block),
              add (count_wbl wblocks.reps) wblock_hashtbl (Nt node2) (NumSet.singleton block) with
            | Tp, _ | _ , Tp -> Tp
            | Nt node1, Nt node2 ->
              let newnode = join_traces (count_wbl wblocks.reps) node1 node2 in
              match newnode with 
              | Tp -> Tp
              | Nt newnode ->
                let reps = if node1.Trie.node_UID = node2.Trie.node_UID then begin
                    assert (newnode.Trie.node_UID = node1.Trie.node_UID);
                    increment_reps wblocks.reps newnode.Trie.node_UID
                  end else
                    NumMap.add newnode.Trie.node_UID (IntSet.singleton 1) wblocks.reps in
                  Nt { node1 = newnode;
                       node2 = None;
                       reps = reps }
      
    (* Hitmiss tracking for touch_hm *)
    let touch_hm env addr rw =
      match env.cache with
      | Tp ->
          let tp_env = { (env) with traces = Tp; times = Tp; }
          in ((Nb tp_env), (Nb tp_env))
      | Nt cache ->
          let (c_hit, c_miss) = CA.touch_hm cache addr rw in
          let record_status status c_env =
            (* Assume that because write-through cache is being used,*)
            (* a write-hit is perceived by attacker as a miss *)
            let status = if (rw = Write) && (status = H) then M else status
            in
              (match c_env with
               | Nb c ->
                   let block_addr = get_block_id env addr
                   in
                     Nb
                       {
                         env with
                         traces =
                           add count_normal hitmiss_hashtbl env.traces
                             (NumSet.singleton (cachest_to_int64 status));
                         cache = Nt c;
                         times = add_time_status status env.times;
                         addr_trace =
                           add count_normal addr_hashtbl env.addr_trace
                             (NumSet.singleton addr);
                         block_trace =
                           add count_normal block_hashtbl env.block_trace
                             (NumSet.singleton block_addr);
                         wblocks = update_wblocks env.wblocks block_addr;
                       }
               | Bot -> Bot)
          in ((record_status H c_hit), (record_status M c_miss))
      
    let touch env addr rw =
      let (env_hit, env_miss) = touch_hm env addr rw
      in
        match (env_hit, env_miss) with
        | (Bot, Bot) -> raise Bottom
        | (Nb e, Bot) | (Bot, Nb e) -> e
        | (Nb e1, Nb e2) -> join e1 e2
      
    let elapse env time =
      let times = add_time time env.times in
      (* elapse is called after each instruction and adds an "N"-node; *)
      (* in the traces two consecutive N's will correspond to "no cache access"*)
      let traces =
        add count_normal hitmiss_hashtbl env.traces
          (NumSet.singleton (cachest_to_int64 N)) in
      let times = add_time_status N times
      in { (env) with times = times; traces = traces; }
      
    let count_cache_states env =
      match env.cache with
      | Nt cache -> CA.count_cache_states cache
      | Tp -> Big_int.zero_big_int
      
  end
  
