(* Copyright (c) 2013-2017, IMDEA Software Institute.             *)
(* See ../../LICENSE for authorship and licensing information     *)

open Big_int
open AD.DS
open NumAD.DS
open AbstrInstr
open Logger

(* This flag can disable an optimization (for precision) *)
(* which checks whether ages of elements are achievable by the *)
(* corresponding replacement strategy.*)
(* If disabled, holes will be counted as possible, if enabled, only the holes*)
(* achievable by the strategy will be counted *)
let exclude_impossible = ref true

module type S = sig
  include AD.S
  val init : int -> (var -> int) -> (var->string) -> t
  val inc_var : t -> var -> t
  val set_var : t -> var -> int -> t
  val delete_var : t -> var -> t
  val permute : t -> (int -> int) -> var -> t
  val get_values : t -> var -> int list
  val exact_val : t -> var -> int -> (t add_bottom)
  val comp : t -> var -> var -> (t add_bottom)*(t add_bottom)
  val comp_with_val : t -> var -> int -> (t add_bottom)*(t add_bottom)
  val var_names : t -> NumSet.t
end


module Make (V: ValAD.S) = struct
  type t = {
    value: V.t;
    max_age : int; (* max_age = the cache associativity 
                      and is interpreted as "outside of the cache" *)
    pfn : var -> int;
  }
   
  let init max_age pfn v2s = {
    value = V.init v2s; 
    max_age = max_age;
    pfn = pfn;
  }
  

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
    let young,nyoung,bigger = vguard env.value v env.max_age in
(* we assume the cases bigger than max_age are irrelevent and we never increase values above max_age *)
    assert (bigger = Bot);
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
    | Tp -> env
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
    
  let var_names env = V.var_names env.value
end

