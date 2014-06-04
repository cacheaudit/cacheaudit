open Big_int
open AD.DS
open NumAD.DS
open AbstrInstr
open Logger

type replacement_strategy = 
  | LRU  (** least-recently used *)
  | FIFO (** first in, first out *)
  | PLRU (** tree-based pseudo LRU *)

module type S = sig
  include AD.S
  val init : int -> (var -> int) -> (var->string) -> replacement_strategy -> t
  val inc_var : t -> var -> t
  val set_var : t -> var -> int -> t
  val delete_var : t -> var -> t
  val permute : t -> (int -> int) -> var -> t
  val get_values : t -> var -> int list
  val exact_val : t -> var -> int -> (t add_bottom)
  val comp : t -> var -> var -> (t add_bottom)*(t add_bottom)
  val comp_with_val : t -> var -> int -> (t add_bottom)*(t add_bottom)
  val get_strategy : t -> replacement_strategy
  val count_cstates: t -> big_int * big_int
end


module Make (V: ValAD.S) = struct
  
  type t = {
    value: V.t;
    max_age : int; (* max_age = the cache associativity 
                      and is interpreted as "outside of the cache" *)
    pfn : var -> int;
    strategy : replacement_strategy
  }
    
  
  let init max_age pfn v2s str = {
    value = V.init v2s; 
    max_age = max_age;
    pfn = pfn;
    strategy = str
  }
  
  let get_strategy env = env.strategy
  
  let join e1 e2 = 
    assert (e1.max_age = e2.max_age);
    {e1 with value = (V.join e1.value e2.value)}
  
  let flatten fmap = 
    assert (FlagMap.cardinal fmap = 1);
    FlagMap.find initial_flags fmap

  let fmap_to_tuple fmap =
    let get_values flgs = try( 
        Nb (FlagMap.find flgs fmap)
      ) with Not_found -> Bot in
     get_values {cf = true; zf = true},
     get_values {cf = true; zf = false},
     get_values {cf = false; zf = true},
     get_values {cf = false; zf = false}
  
  (* computes comparison of x1 and x2, see vguard below *)
  (* the first result is x1<x2, the second one x1=x2 and the last one x1>x2 *)
  let vcomp venv x1 x2 = 
    let _,tf,ft,ff= fmap_to_tuple (V.update_val venv initial_flags x1 NoMask x2 NoMask (Aflag Acmp) None) in
    (* Since we only have positive values, when the carry flag is set, it means venv is strictly smaller than x2 *)
    (* The case where there is a carry and the result is zero should not be 
possible, so it approximates Bottom *)
   tf,ft,ff

  (* Compute x<c in V, where x is a variable and c an int. Should be extended to the case where c is a var *)
  (* the first result is x<c, the second one x=c and the last one x>c *)
  let vguard venv x c = vcomp venv x (Cons(Int64.of_int c))

  let inc_var env v = 
    let young,nyoung,_ = vguard env.value v env.max_age in
(* we assume the cases bigger than max_age are irrelevent and we never increase values above max_age *)
    let new_valad = 
      match young with
      | Bot -> env.value
      | Nb yenv ->
        let yenv = flatten (V.update_val yenv initial_flags v NoMask (Cons 1L) NoMask (Aarith X86Types.Add) None) in
        match nyoung with
        | Bot -> yenv
        | Nb nyenv -> V.join yenv nyenv
    in {env with value = new_valad}
                       
  let is_var env a = V.is_var env.value a

  let set_var env v a = 
      (* set_var cannot set to values greater than the maximal value *)
      assert (a <= env.max_age);
      let value = if not (is_var env v) then 
                  V.new_var env.value v
                else env.value
      in let value = flatten(V.update_val value initial_flags v NoMask (Cons(Int64.of_int a)) NoMask Amov None) in
      {env with value = value}
  
  
  let list_max l = List.fold_left (fun m x -> if x > m then x else m ) 0L l
 
  (* updates an env according to the value env *)
  let vNewEnv env = function
    Bot -> Bot
  | Nb venv -> Nb{env with value = venv}

  (* the first result is approximates the cases when x1 < x2 and
     the second one when x1 > x2 *)
  let comp env x1 x2 = 
    let smaller, _, bigger = vcomp env.value x1 (VarOp x2) in
    vNewEnv env smaller, vNewEnv env bigger

  let comp_with_val env x v =
    let smaller, eq, bigger = vguard env.value x v in
    vNewEnv env smaller, lift_combine join (vNewEnv env eq) (vNewEnv env bigger)

  let exact_val env x c =
    let smaller, eq, bigger = vguard env.value x c in vNewEnv env eq

  (* apply the permutation perm to the age of variable x *)
  let permute env perm x = 
    let perm64 a = Int64.of_int (perm (Int64.to_int a)) in
    match V.get_var env.value x with
      Tp -> env
    | Nt vm -> 
      let v1,_ = NumMap.min_binding vm in
      let value1 = let nv1 = perm64 v1 in V.set_var env.value x nv1 nv1 in 
      {env with value = 
           NumMap.fold (fun c _ value -> let nc = perm64 c in
                        V.join value (V.set_var value x nc nc))
                     (NumMap.remove v1 vm) value1
      }
  

  let print_delta env1 fmt env2 = V.print_delta env1.value fmt env2.value
  let print fmt env = 
    V.print fmt env.value

  let subseteq env1 env2= 
    assert (env1.max_age = env2.max_age);
    (V.subseteq env1.value env2.value)
  let widen env1 env2 = 
    assert (env1.max_age = env2.max_age);
    {env1 with value = (V.widen env1.value env2.value)}

  let get_values env v = let l = match V.get_var env.value v with
     Tp -> []  | Nt x -> NumMap.bindings x in
     List.map (fun (k,_) -> Int64.to_int k) l
  
  let delete_var env v = {env with value = V.delete_var env.value v}
    
  
  (*** Counting valid states ***)
  
  (* Checks if the given cache state is valid *)
  (* with respect to the ages (which are stored in [env])*)
  (*  of the elements of the same cache set [addr_set]*)
  let is_valid_cstate env addr_set cache_state  = 
    assert (List.for_all (function Some a -> NumSet.mem a addr_set | None -> true) cache_state);
    (* get the position of [addr] in cache state [cstate], starting from [i];*)
    (* if the addres is not in cstate, then it should be max_age *)
    let rec pos addr cstate i = match cstate with 
       [] -> env.max_age
    | hd::tl -> if hd = Some addr then i else pos addr tl (i+1) in
    NumSet.for_all (fun addr -> 
      List.mem (pos addr cache_state 0) (get_values env addr)) addr_set 
  
  
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
  (* Additionally, return the set of n-subsets (the valid permutations without order) *)
  let num_tuples env n addr_set = 
    if NumSet.cardinal addr_set >= n then begin
      (* the loop creates all n-permutations and tests each for validity *)
      let rec loop n elements tuple num sets = 
        if n = 0 then 
          if env.strategy <> PLRU then
            if is_valid_cstate env addr_set tuple then 
              Int64.add num 1L,NumSetSet.add (numset_from_list tuple) sets else num,sets
          else
            (* In PLRU "holes" are possible, i.e., there may be a with age i, *)
            (* and there is no b with age i-1. *)
            (* We overapproximate the counting for this by considering all *)
            (* possible combinations of holes *)
            (* (even though not all are achievable with PLRU).*)
            (* In the current tuple, we try all possibilities to put holes,*)
            (* and test the new tuples (cstate-s) for validity. *)
            (* There are (max_age choose |tuple|) possible ways to put the holes *)
            (* which for associativity 4 is at most 6, for 8 at most 70 *)
            let rec loop_holes cstate rem_elts num sets = 
              if (List.length rem_elts) = 0 then 
                (* no rem_elts -> this is a possible cache state;*)
                (* check it for validity *)
                if is_valid_cstate env addr_set cstate then Int64.add num 1L, NumSetSet.add (numset_from_list tuple) sets else num, sets
              else
                let elt = List.hd rem_elts in
                let rem_elts = List.tl rem_elts in
                (* a list containing the possible number of holes before elt *)
                let poss_num_holes = create_range 0 (env.max_age - 
                  (List.length cstate) - (List.length rem_elts)) in
                
                List.fold_left (fun (num,sets) nholes -> 
                  (* add the nholes holes and elt to the state *)
                  (* and continue scanning the remaining elements *)
                  loop_holes (cstate @ (create_ulist nholes None) @ [elt]) rem_elts num sets
                  ) (num,sets) poss_num_holes
            in loop_holes [] tuple num sets
        else
          (* add next element to tuple and continue looping *)
          (* (will go on until n elements have been picked) *)
          NumSet.fold (fun addr (s1,s2) -> 
            loop (n-1) (NumSet.remove addr elements) ((Some addr)::tuple) s1 s2
            ) elements (num, sets) in 
      loop n addr_set [] 0L NumSetSet.empty
    end else 0L,NumSetSet.empty
    
  (* Computes two lists where each item i is the number of possible *)
  (* cache states of cache set i for a shared-memory *)
  (* and the disjoint-memory (blurred) adversary *)
  let cache_states_per_set env =
    let cache_sets = Utils.partition (V.var_names env.value) env.pfn in
    IntMap.fold (fun _ addr_set (nums,bl_nums) ->
        let num_tpls,num_bl =
          let rec loop i (num,num_blurred) =
            if i > env.max_age then (num,num_blurred)
            else
              let this_num, sets = 
                num_tuples env i addr_set in 
              let this_bl = Int64.of_int (NumSetSet.cardinal sets) in
              loop (i+1) (Int64.add num this_num, Int64.add num_blurred this_bl) in 
          loop 0 (0L,0L) in 
        (num_tpls::nums,num_bl::bl_nums)
      ) cache_sets ([],[])
      
  let count_cstates env = 
    let nums_cstates,bl_nums_cstates = cache_states_per_set env in
      (Utils.prod nums_cstates,Utils.prod bl_nums_cstates)
end

