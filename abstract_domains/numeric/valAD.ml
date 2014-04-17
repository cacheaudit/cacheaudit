open X86Types
open AbstrInstr
open AD.DS
open NumAD.DS
open Logger

(* We use a module for the options so that we can have different instances in the same analysis *)
module type VALADOPT = sig
  val max_get_var_size:int
  val max_set_size:int
end

module ValADOptForMemory = struct
  let max_get_var_size = 256
  let max_set_size = 32
end

let logFile = ref None

module type S = sig 
  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
  val log_var : var -> t -> unit
  val get_var : t -> var -> (t NumMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val is_var : t -> var -> bool
  val var_names : t -> NumSet.t
  val meet : t -> t -> t add_bottom 
  val update_val : t -> flags_t -> var -> mask -> cons_var -> mask -> 
    AbstrInstr.abstr_op -> t FlagMap.t 
end


module Make (O:VALADOPT) = struct
(* A basic variable contains a 32 bits unsigned integer. *)

  (* Type of the abstract elements representing one variable *)
  type var_t = FSet of NumSet.t | Interval of int64*int64

  let min_elt = 0L
  let max_elt = 0xffffffffL
  let two32 = 0x100000000L
  let top = Interval(min_elt, max_elt)
  let is_top l h = l=min_elt && h=max_elt

  let rec interval_to_set l h = if l>h then NumSet.empty
    else NumSet.add l (interval_to_set (Int64.succ l) h)
  let set_to_interval s = Interval(NumSet.min_elt s, NumSet.max_elt s)
  let zero = FSet(NumSet.singleton 0L)
  let range_over (l,h) r =
    let range = Int64.sub h l in
    range > (Int64.of_int max_int) || Int64.to_int range > r 

  type t = var_t VarMap.t

  let var_names env = 
    let keys,_ = List.split (VarMap.bindings env) in 
    List.fold_left (fun set elt -> NumSet.add elt set) NumSet.empty keys

  
(* TODO put this into the type t *)
  let variable_naming = ref(fun x -> "")

  let pp_var_vals fmt = function
  | FSet vals -> Format.fprintf fmt "@[{";
      NumSet.iter (fun v -> Format.fprintf fmt "%Ld @," v) vals;
      Format.fprintf fmt "}@]"
  | Interval(l,h) -> if is_top l h then Format.fprintf fmt "Top"
                     else Format.fprintf fmt "@[[%Ld, %Ld]@]" l h

  let var_vals_equal x1 x2 = match x1,x2 with
  | FSet s1, FSet s2 -> NumSet.equal s1 s2
  | Interval(l1,h1), Interval(l2,h2) -> l1=l2 && h1=h2
  | FSet s1, Interval(l2,h2) | Interval(l2,h2), FSet s1 ->
      NumSet.min_elt s1 = l2 && NumSet.max_elt s1 = h2 && 
      NumSet.equal s1 (interval_to_set l2 h2)

  let print_one_var fmt v vals  = Format.fprintf fmt "@[%s in %a@]@;"
    (!variable_naming v) pp_var_vals vals

  let log_var v env= 
    let file = match !logFile with
      | None -> let f = (open_out "log.txt") in logFile := Some (f); f
      | Some f -> f
    in let log_var_vals = function
      | FSet vals -> Printf.fprintf file "{";
          NumSet.iter (fun v -> Printf.fprintf file "%Ld " v) vals;
          Printf.fprintf file "}"
      | Interval(l,h) -> if is_top l h then Printf.fprintf file "Top"
                         else Printf.fprintf file "[%Ld, %Ld]" l h
    in Printf.fprintf file "%s " (!variable_naming v);
    log_var_vals (VarMap.find v env);
    Printf.fprintf file "\n"

  let print fmt env = Format.fprintf fmt "@[<hov 0>";
    VarMap.iter (print_one_var fmt) env;
    Format.fprintf fmt "@]"

  (* We just print differing and new variables *)
  let print_delta env1 fmt env2 = match get_log_level ValLL with
    | Debug -> Format.fprintf fmt "@[";
           VarMap.iter (fun v vals -> try 
             let old_vals = VarMap.find v env1  in
             if not(var_vals_equal old_vals vals) then  print_one_var fmt v vals
             with Not_found -> print_one_var fmt v vals) env2;
           Format.fprintf fmt "@]"
    | _ -> ()

  (* truncate a [number] to its [numbits] least significant bits *)
  let truncate numbits number = 
    match numbits with
      | 32 -> Int64.logand number 0xFFFFFFFFL
      | 8 -> Int64.logand number 0xFFL
      | _ -> failwith "truncate: wrong argument"

  let set_map f set = NumSet.fold (fun x -> NumSet.add (f x)) set NumSet.empty

(* Mask values and shift them to the right *)
  let apply_mask mask shift x =
    Int64.shift_right (Int64.logand mask x) shift

  let mask_vals mask v = match mask with
  | NoMask -> v
  | Mask msk ->
   let (cv_mask, cv_shift) = mask_to_intoff msk in
   let am = apply_mask cv_mask cv_shift in
   match v with
   | FSet s -> FSet(set_map am s)
   | Interval(l,h) -> 
      let ml = if h>cv_mask then 0L else am l in
      let mh = if l>cv_mask then 0L (*in that case we have also h>cv_mask *)
        else if h>cv_mask then am cv_mask
        else am h in
      assert(ml<=mh);
      Interval(ml,mh)

  (* Flag setting functions *)
  let flag_carry res =
    let upper_bound = Int64.shift_left Int64.one 32 in
    let lower_bound = 0L in
    res < lower_bound || res >= upper_bound
  let flag_carry_shift sop original result offset =
    let carry = match sop with
      Shl | Rol -> Int64.logand result (Int64.shift_left Int64.one 32)
    | Shr | Sar | Ror ->
        let mask = Int64.shift_left Int64.one (offset - 1) in
        Int64.logand mask original
    in
    carry <> Int64.zero
  let flag_zero res = ((truncate 32 res) = 0L)

  (* set_or_interval given two bounds either returns an interval or a set if its size is less than O.max_set_size *)
  let set_or_interval l h = if range_over (l,h) O.max_set_size then Interval(l,h) else FSet (interval_to_set l h)

  let init v2s = variable_naming:=v2s; VarMap.empty

  let new_var m v = VarMap.add v top m

  let delete_var m v = VarMap.remove v m

  let is_var m vn = VarMap.mem vn m

  let get_var m v = try (match VarMap.find v m with
    | FSet l -> Nt (NumSet.fold (fun vl env -> NumMap.add vl (VarMap.add v (FSet (NumSet.singleton vl)) m) env) l NumMap.empty)
    | Interval(l,h) -> if range_over (l,h) O.max_get_var_size then Tp
        else
          let rec f l h = if l>h then NumMap.empty
            else NumMap.add l (VarMap.add v (FSet (NumSet.singleton l)) m) 
                   (f (Int64.succ l) h) in
          Nt(f l h)
  ) with Not_found -> failwith  (Printf.sprintf "valAD.get_var: non-existent variable %Lx\n" v)

  let set_var m v l h = VarMap.add v (set_or_interval l h) m

  let get_vals c m = match c with
    Cons cs -> FSet (NumSet.singleton (truncate 32 cs))
  | VarOp v -> try VarMap.find v m
               with Not_found -> failwith "valAD.get_vals: inexistent variable"

  let same_vars v cv = match cv with VarOp v2 -> v=v2 | Cons _ -> false

  let var_join x y = match x,y with
  | FSet s1, FSet s2 -> let s = NumSet.union s1 s2 in
      if NumSet.cardinal s > O.max_set_size then set_to_interval s else FSet s
  | Interval(l,h), FSet s | FSet s, Interval(l,h) -> 
      Interval(min l (NumSet.min_elt s), max h (NumSet.max_elt s))
  | Interval(l1,h1), Interval(l2,h2) -> Interval(min l1 l2, max h1 h2)

  let join (x:t) (y:t) =
    let f k a b = match a,b with
        | Some c, Some d -> Some(var_join c d)
        (* f should never enter this case *)
        | _, None | None, _ -> failwith "Disjoint variables in valAD.join"
    in
    VarMap.merge f x y
  
  let var_meet x y = match x,y with
  | FSet s1, FSet s2 -> let s = NumSet.inter s1 s2 in
      if NumSet.is_empty s then raise Bottom else FSet s
  | Interval(l,h), FSet s | FSet s, Interval(l,h) -> 
      let rs = NumSet.filter (fun x -> x>=l && x<=h) s in
      if NumSet.is_empty rs then raise Bottom else FSet rs
  | Interval(l1,h1), Interval(l2,h2) ->
      let l = max l1 l2 in
      let h = min h1 h2 in
      if l > h then raise Bottom 
      else if range_over (l,h) O.max_set_size then Interval(l,h)
      else FSet(interval_to_set l h)

  let meet x y =
    let f k a b = match a,b with
        | Some c, Some d -> Some (var_meet c d)
        (* f should never enter this case *)
        | _, None | None, _ -> failwith "Disjoint variables in valAD.meet"
    in
    try Nb (VarMap.merge f x y)
    with Bottom -> Bot

  let var_widen x y = match x, y with
  | FSet s1, FSet s2 -> let s = NumSet.union s1 s2 in
      if NumSet.cardinal s > O.max_set_size then set_to_interval s else FSet s
  | Interval(l,h), FSet s | FSet s, Interval(l,h) ->
      Interval(min l (NumSet.min_elt s), max h (NumSet.max_elt s))
  | Interval(l1,h1), Interval(l2,h2) -> 
     let l = if l2<l1 then min_elt else l1 in
     let h = if h2>h1 then max_elt else h1 in
     Interval(l,h)

  let widen x y =
    (*let pp_one_var v fmt k = print_one_var fmt k v in*)
    let f k a b = match a,b with
        | Some c, Some d -> Some(var_widen c d)
        (* f should never enter this case *)
        | _, None | None, _ -> failwith "Disjoint variables in valAD.widen"
    in
    VarMap.merge f x y

  let var_subseteq x y = match x,y with
 | FSet s1, FSet s2 -> NumSet.subset s1 s2
 | FSet s, Interval(l,h) -> NumSet.min_elt s >= l && NumSet.max_elt s <= h
 | Interval(l,h), FSet s -> 
    let res = ref true in
    let i = ref l in
    while(!res && !i<=h) do res:=NumSet.mem !i s; i:= Int64.succ !i done;
    !res
 | Interval(l1,h1), Interval(l2,h2) -> l1>=l2 && h1<=h2

  exception Not_included
  let subseteq x y =
    let f k a b = match a,b with
      | Some c, Some d -> if var_subseteq c d then None else raise Not_included
       | Some c, _ -> raise Not_included
       | _, Some d -> None
       | _, _ -> None
     in
     try (ignore (VarMap.merge f x y); true)
     with Not_included -> false

  (* Functions used by update_val *)
  let arithop_to_int64op = function
    | Add -> Int64.add
    | Addc -> Int64.add
    | And -> Int64.logand
    | CmpOp -> failwith "CmpOp should be handled by flagop instead of memop"
    | Or -> Int64.logor
    | Sub -> Int64.sub
    | Subb -> Int64.sub
    | Xor -> Int64.logxor


  let make_set_from_list = List.fold_left (fun s x -> NumSet.add x s) NumSet.empty
  
  (* lifts to interval representation if needed *)
  let lift_to_interval s = 
    if NumSet.cardinal s > O.max_set_size then set_to_interval s else FSet s

  type position = TT | TF | FT | FF


  (* Wrapper function for var_meet; we need the intersection without raising Bottom *)
  let var_meet_ab x y = try (Nb (var_meet x y)) with Bottom -> Bot

  (* Applies op to each element of the FSet or the bounds of the Interval *)
  let mapt op = function
    | FSet s -> FSet (set_map op s)
    | Interval(l,h) -> Interval(op l, op h)

  (* Functions for bitwise interval operations 
   * Based on http://www.cs.utah.edu/~regehr/papers/lctes06_2/ *)
  let coef x k =
    let twok = Int64.shift_left 1L k in
    Int64.div x twok

  (* Zeroes the k-th bit of the interval *)
  let killBit interval k =
    let even x = (Int64.rem x 2L = 0L) in
    let twok = Int64.shift_left 1L k in
    match interval with
    | Interval(l,h) -> begin
        let ch = coef h k in
        let cl = coef l k in
        let ub = 
          if even ch then h
          else if l < (Int64.mul ch twok) then Int64.mul ch twok
          else Int64.sub h twok
        in
        let lb =
          if not (even cl) then Int64.sub l twok
          else if h >= Int64.mul twok (Int64.succ cl) then Int64.mul cl twok
          else l
        in
        Interval(lb,ub)
    end
    | _ -> raise (Invalid_argument "killBit")

  let isBitSet x bit = (Int64.logand (Int64.shift_left 1L bit) x) <> Int64.zero

  let get_bounds = function
    | Interval (l,h) -> (l,h)
    | _ -> raise (Invalid_argument "get_bounds")

  (* Function to compute the "contributing" bits of the interval *)
  let loopBound ai bi bound =
    let testbound = bound (get_bounds bi) in
    let newai = ref ai in
    for i = 31 downto 0 do
      if isBitSet testbound i
        then newai := killBit !newai i
    done;
    bound (get_bounds !newai)

  (* Bitwise or for intervals *)
  let interval_or a b =
    match a,b with
    | Interval(al,ah), Interval (bl,bh) ->
        let lma,lmb = loopBound a b fst, loopBound b a fst in
        let hma,hmb = loopBound a b snd, loopBound b a snd in
        let lowerBound = min (Int64.logor al lmb) (Int64.logor bl lma) in
        let upperBound = max (Int64.logor ah hmb) (Int64.logor ah hma) in
        Interval (lowerBound, upperBound)
    | _,_ -> raise (Invalid_argument "interval_or")

  (* Bitwise not for intervals *)
  let interval_not = function
    | Interval(l,h) ->
        let f x = truncate 32 (Int64.lognot x) in
        Interval(f h, f l)
    | _ -> raise (Invalid_argument "interval_not")

  (* Bitwise and for intervals defined using or and not *)
  let interval_and a b =
    (* a & b = not (not a | not b) *)
    interval_not (interval_or (interval_not a) (interval_not b))

  (* interval_operation takes an operation and two intervals and returns the resulting
   * interval after applying the operation (which must be monotonic) *)
  let interval_operation oper x y = match x,y with 
    | Interval (dl,dh), Interval (sl,sh) ->
        let bound f = List.fold_left f (oper dl sl) [oper dl sh; oper dh sl; oper dh sh] in
        let lb,ub = bound min, bound max in
        assert(ub >= lb); Interval (lb,ub)
    | _,_ -> raise (Invalid_argument "interval_operation")

  (* interval_arith takes a flg : position = {TT,...,FF}, an arith_op aop,
   * the Int64 operation oper and two intervals *)
  let interval_arith aop oper di si = 
    let retmap = FlagMap.empty in
    (* Interval after operation *)
    let inter = match aop with 
    | Add | Addc | Sub | Subb -> interval_operation oper di si 
    | And | Or | Xor -> begin match aop with 
        | And -> interval_and di si
        | Or -> interval_or di si
        | Xor -> (* a xor b = (a & not b) or (b & not a) *)
          let f a b = interval_and a (interval_not b) in
          interval_or (f di si) (f si di) 
        | _ -> assert false 
        end
    | _ -> assert false in
    
    (* Can set ZF only if result only contains zero *)
    let retmap = match var_meet_ab inter (FSet (NumSet.singleton min_elt)) with
    | Nb z -> FlagMap.add {cf = false; zf = true} z retmap 
    | _ -> retmap in
    
    let meetNormal = var_meet_ab inter (Interval (Int64.succ min_elt, max_elt)) in
    (* Can set not CF and not ZF if interval is in normal range *)
    (* not CF not ZF if no zero and no under *)
    let retmap = match meetNormal with
    | Nb n -> FlagMap.add {cf = false; zf = false} n retmap
    | _ -> retmap in
    
    let modulo_add = fun x -> Int64.sub x two32 in
    let modulo_sub = fun x -> Int64.add two32 x in
    let retmap = match aop with 
    | Add | Addc -> begin 
        (* Can set CF & ZF only if result is 2^32 *)
        match var_meet_ab inter (FSet (NumSet.singleton two32)) with
        | Nb z2 -> FlagMap.add {cf = true; zf = true} (mapt modulo_add z2) retmap
        | _ -> retmap 
      end
    | _ -> retmap in
    
    let retmap = match aop with
    | Add | Addc -> 
      let meetOver = var_meet_ab inter (Interval (Int64.succ two32, Int64.sub (Int64.add two32 two32) 2L)) in
      (* Can set CF & not ZF if ub is > than 2^32 and does not contain zero *)
      begin match meetNormal,meetOver with
      | Nb n,Nb o -> FlagMap.add {cf = true; zf = false} (var_join n (mapt modulo_add o)) retmap
      | Bot,Nb o -> FlagMap.add {cf = true; zf = false} (mapt modulo_add o) retmap 
      | _, _ -> retmap end
    | Sub | Subb ->
      let meetUnder = var_meet_ab inter (Interval (Int64.pred min_elt, Int64.sub 1L two32)) in
      (* CF not ZF if under *)
      begin match meetUnder with
      | Nb u -> FlagMap.add {cf = true; zf = false} (mapt modulo_sub u) retmap
      | _ -> retmap end
    | _ -> retmap in
    
    if FlagMap.is_empty retmap then raise Bottom 
      else retmap
    
  let to_interval = function 
    | FSet s -> (set_to_interval s)
    | i -> i

  let bool_to_int64 = function true -> 1L | false -> 0L
  
  (* let fmap_set_default key defval fm =  *)
  (*   if (FlagMap.mem key fm) then fm     *)
  (*   else (FlagMap.add key defval fm)    *)
  let fmap_find_with_defval key defval fm = try(
    FlagMap.find key fm) with Not_found -> defval

  
  (* Implements the effects of arithmetic operations *)
  let arith m flgs_init dstvar mkvar dst_vals srcvar mkcvar src_vals aop = 
    match aop with
    | Xor when same_vars dstvar srcvar -> (*Then the result is 0 *)
              FlagMap.singleton {cf = false; zf = true} (VarMap.add dstvar zero m)
    | _ ->
    if (mkvar, mkcvar) <> (NoMask,NoMask) then 
      failwith "ValAD.arith: only 32-bit arithmetic is implemented"
    else
              (* Add carry flag value to operation if it's either Addc or Subb *)
              let oper x y = let y = match aop with 
                | Addc | Subb -> 
                  Int64.add y (bool_to_int64 flgs_init.cf)
                | _ -> y in
                arithop_to_int64op aop x y in
              begin
                (* returns a set of dst op src for one dst and all possible *)
                (* src in ss, which produce a particular flag combination *)
                  let compute_op_set dst sset = NumSet.fold (fun src fm -> 
                      let result = oper dst src in
                      let flags = {cf = flag_carry result; zf = flag_zero result} in
                      let vals = fmap_find_with_defval flags NumSet.empty fm in
                      FlagMap.add flags (NumSet.add (truncate 32 (result)) vals) fm
                    ) sset FlagMap.empty in
                  let finalSetMap = 
                    match dst_vals, src_vals with
                    | FSet dset, FSet sset ->
                      let fsmap = NumSet.fold (fun dst s -> 
                        fmap_combine (compute_op_set dst sset) s NumSet.union) dset FlagMap.empty in
                      FlagMap.map (fun nums -> lift_to_interval nums) fsmap
                    | _, _ ->
                  (* We convert sets into intervals in order to perform the operations;
                   * interval_arith may return either FSet or Interval *)
                      interval_arith aop oper (to_interval dst_vals) (to_interval src_vals)
                    
                  in FlagMap.map (fun nums -> VarMap.add dstvar nums m) finalSetMap
              end


  (* Implements the effects of MOV *)
  let mov env flags dstvar mkvar dst_vals srcvar mkcvar src_vals =
    let new_env = match mkvar, mkcvar with
    (* 32 bits -> 32 bits MOV and 8 -> 32 : MOVZX *)
    | NoMask, msk ->
        let src_vals = mask_vals msk src_vals in
        (VarMap.add dstvar src_vals env)
    (* 8 -> 8 : MOVB *)
    | Mask mkv, Mask mkc ->
            begin
                match dst_vals, src_vals with
                | FSet ds, FSet ss_unmasked ->
                    let (c_mask, c_shift) = mask_to_intoff mkc in
                    (* Nullify everything but the 8 bits corresponding to the mask *)
                    let ss_shifted = set_map (fun x -> Int64.logand x c_mask) ss_unmasked in
                    (* Then shift it to the first 8 bits *)
                    let ss = set_map (fun x -> Int64.shift_right x c_shift) ss_shifted in
                    let (v_mask, v_shift) = mask_to_intoff mkv in
                    (* Align cv values in order to write them into v *)
                    let cvSet = set_map (fun x -> Int64.shift_left x v_shift) ss in
                    (* Nullify the 8 bits corresponding to the mask *)
                    let varSet = set_map (fun x -> Int64.logand (Int64.lognot v_mask) x) ds in
                    (* Create a list of set combining all the posible values with the mask in place *)
                    let doOp x = set_map (fun y -> Int64.logor x y) varSet in
                    let setList = NumSet.fold (fun x r -> doOp x :: r) cvSet [] in
                    (* Unite all the sets *)
                    let finalSet = List.fold_left NumSet.union NumSet.empty setList in
                    (VarMap.add dstvar (lift_to_interval finalSet) env)
                | _, _ -> (VarMap.add dstvar top env)
              end
    | _, _ -> failwith "ValAD.move: operation from 32 bits to 8 bits" in 
  FlagMap.singleton flags new_env

  (* interval_flag_test takes two intervals and a flag combination and returns
   * the corresponding intervals *)
  let interval_flag_test pos op di si =
    match op with
    | Sub ->
        let dl,dh = get_bounds di in
        let sl,sh = get_bounds si in
        (* Intersection between the two intervals *)
        let meetZF = var_meet_ab di si in
        begin
          match pos with
          | TT -> raise Bottom
          | TF -> (* We should have [dl, min(dh,sh-1)] and [max(dl+1,sl), sh] *)
              let ndh = min dh (Int64.pred sh) in
              let nsl = max (Int64.succ dl) sl in
              if ndh < dl || nsl > sh then raise Bottom
              else (set_or_interval dl ndh),(set_or_interval nsl sh)
          | FT -> (* meet <> empty then ZF can be set *)
              begin
                match meetZF with
                | Bot -> raise Bottom
                | Nb z -> z,z
              end
          | FF -> 
              let nsh = min (Int64.pred dh) sh in
              let ndl = max dl (Int64.succ sl) in
              if ndl > dh || nsh < sl then raise Bottom
              else (set_or_interval ndl dh),(set_or_interval sl nsh)
        end
    | _ -> failwith "interval_flag_test: TEST instruction for intervals not implemented yet"

  let flagop m flags fop dst dvals src svals =
    match dvals,svals with
      FSet dvals, FSet svals ->
        let op = arithop_to_int64op fop in
        (* Combines two sets of values and only keeps those combinations that satisfy test;
         * returns the tuples with the values that satisfy test *)
        let setComb d s test =
          let doOp x = NumSet.fold (fun y r -> if test (op y x) then (y,x) :: r else r) d [] in
          List.concat (NumSet.fold (fun x r -> (doOp x) :: r) s [])
        in
        (* Create an environment given a test function *)
        let create_m test =
        (* Do all the possible combinations *)
          let combs = setComb dvals svals test in
          (* If combs is empty, it means that there are no values that satisfy test.
           * Then we return Bottom *)
          if combs = []
            then Bot
            else Nb (
              (* Create the enviroment by adding the values obtained to the VarMap *)
              let ld,ls = List.split combs in
              let new_m = VarMap.add dst (FSet (make_set_from_list ld)) m in
              (match src with
              | VarOp x -> VarMap.add x (FSet (make_set_from_list ls)) new_m
              | Cons _ -> new_m)
            )
        in 
        tupleold_to_fmap (
          create_m (fun x -> flag_carry x && flag_zero x),
          create_m (fun x -> flag_carry x && not (flag_zero x)),
          create_m (fun x -> not (flag_carry x) && flag_zero x),
          create_m (fun x -> not (flag_carry x) && not (flag_zero x))
        )
    | _,_ ->
        let ift pos = 
          interval_flag_test pos fop (to_interval dvals) (to_interval svals) in
        (* Add the interval of dst to the enviroment *)
        let newm pos = VarMap.add dst (fst (ift pos)) m in
        (* If src is a variable, add interval of src to env *)
        let create_m pos = 
          (match src with
          | VarOp x -> VarMap.add x (snd (ift pos)) (newm pos)
          | Cons _ -> newm pos)
        in
        let finalm pos = try (Nb (create_m pos)) with Bottom -> Bot in
        tupleold_to_fmap (finalm TT, finalm TF, finalm FT, finalm FF)


  let rotate_left value offset =
    let bin = Int64.shift_left value offset in
    let bout = Int64.shift_right_logical value (32 - offset) in
    Int64.logor (truncate 32 bin) (truncate 32 bout)

  let rotate_right value offset =
    let bin = Int64.shift_right_logical value offset in
    let bout = Int64.shift_left value (32 - offset) in
    Int64.logor (truncate 32 bin) (truncate 32 bout)

  let shift_operator = function
    | Shl -> fun x o -> Int64.shift_left x (Int64.to_int o)
    | Shr -> fun x o -> Int64.shift_right_logical x (Int64.to_int o)
    (* In order to preserve the sign, we need to convert to int32 *)
    | Sar -> fun value off -> Int64.of_int32 (Int32.shift_right (Int64.to_int32 value) (Int64.to_int off))
    | Ror -> fun x o -> rotate_right x (Int64.to_int o)
    | Rol -> fun x o -> rotate_left x (Int64.to_int o)
  
  (* Shift the values of the variable dst using the offsets given by soff *)
  let shift m flags sop dstvar vals soff off_vals mk = 
    let offsets = mask_vals mk off_vals in
    let operator = shift_operator sop in
    let top_env = (VarMap.add dstvar top m) in
      match vals, offsets with
      | FSet dset, FSet offs_set ->
          let compute_op dst = NumSet.fold 
          (fun offs fm -> 
            let result = operator dst offs in
            let flags = {cf = flag_carry_shift sop result dst (Int64.to_int offs); 
              zf = flag_zero result} in
            (* NumSet.add (flags,(truncate 32 result)) s *)
            let vals = fmap_find_with_defval flags NumSet.empty fm in
            FlagMap.add flags (NumSet.add (truncate 32 (result)) vals) fm
            ) offs_set FlagMap.empty in
          
          let finalSetMap = NumSet.fold (fun dst s -> 
            fmap_combine (compute_op dst) s NumSet.union) dset FlagMap.empty in
          let finalSetMap = 
            FlagMap.map (fun nums -> lift_to_interval nums) finalSetMap in
          (* let finalSet = NumSet.fold (fun dst s -> NumSet.fold NumSet.add (perform_op dst) s) dset NumSet.empty in *)
          FlagMap.map (fun nums -> VarMap.add dstvar nums m) finalSetMap
      | Interval(l,h), FSet offs_set -> (* This doesn't work with rotate; return Top if rotate *)
        let newenv =
          let bound f b sup = NumSet.fold (fun offs r -> f (operator b offs) r) offs_set sup in
          let lb, ub = bound min l max_elt, bound max h min_elt in
          (* TODO : flag test *)
          (* Flags are not changed at all which is not correct! *)
          begin
            match sop with
              Ror | Rol -> top_env
            | _ -> if ub < lb then m else VarMap.add dstvar (Interval(lb,ub)) m
          end in
        FlagMap.singleton flags newenv
      | _, _ -> 
        tupleold_to_fmap (Nb top_env, Nb top_env, Nb top_env, Nb top_env)
        
  let update_val env flags dstvar mkvar srcvar mkcvar op = 
    let dvals = VarMap.find dstvar env in
    let svals = get_vals srcvar env in
    match op with
    | Aarith aop -> arith env flags dstvar mkvar dvals srcvar mkcvar svals aop
    | Amov -> mov env flags dstvar mkvar dvals srcvar mkcvar svals
    | Ashift sop -> shift env flags sop dstvar dvals srcvar svals mkcvar
    | Aflag fop -> begin
        match fop with
        | Atest -> flagop env flags And dstvar dvals srcvar svals
        | Acmp -> flagop env flags Sub dstvar dvals srcvar svals
      end
    | Aimul -> failwith "IMUL not implemented yet"
    | _ -> assert false
  
end

