(* Copyright (c) 2013-2017, IMDEA Software Institute.             *)
(* See ../../LICENSE for authorship and licensing information     *)

open X86Types
open AbstrInstr
open AD.DS
open NumAD.DS
open Logger
open Utils

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

let predecessor_map = ref TupleMap.empty

module type S = sig 
  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
  val log_var : var -> t -> unit
  val get_var : t -> var -> (t NumMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val set_symbolic : t -> var -> int64 option -> t * int64
  val is_var : t -> var -> bool
  val var_names : t -> NumSet.t
  val meet : t -> t -> t add_bottom 
  val update_val : t -> flags_t -> var -> mask -> cons_var -> mask -> 
    AbstrInstr.abstr_op -> int64 option -> t FlagMap.t 
  val updval_set : t -> flags_t -> var -> mask -> X86Types.cc ->t FlagMap.t 
  val perform_one_arith : X86Types.arith_op -> bool -> var -> var -> var
end


module Make (O:VALADOPT) = struct
(* A basic variable contains a 32 bits unsigned integer. *)

  (* Type of the abstract elements representing one variable *)
  type var_t = FSet of NumSet.t | Interval of int64*int64

  let min_elt = 0L
  let max_elt = 0xffffffffL
  let two32 = 0x100000000L
  
  (* Mask values and shift them to the right *)
  let apply_mask mask shift x =
    Int64.shift_right (Int64.logand mask x) shift
  
  (* Support for symbolic values *)
  exception Symb_Overflow
  let symb_uid = ref 0
  let max_uid = 0x7ffff
  let get_fresh_uid () = let uid = !symb_uid in
    if uid > max_uid then raise Symb_Overflow
    else incr symb_uid; 
    uid 
  
    
  
  (* [encode_symb id s num] will create a symbolic value with id [id] such that *)
  (* [num] bits are known, and the least significant known bit is the [s]-th; *)
  (* the known part of the value is contained in [v] *)
  let encode_symb uid s num v =
    (* format of the encoded value: 
  [ 01 | uid (20 bit) | s (5 bit) | num (5 bit) |    value (32bit)          ] *)
    assert (num + s <= 32 && num >= 0 && s >= 0);
    assert (uid >= 0 && uid <= max_uid);
    assert (v >= min_elt && v <= max_elt);
    if num = 32 then v 
    else 
        let pos = 64 - 2 in
        let res = Int64.shift_left 1L pos in
        let pos = pos - 20 in
        let res = Int64.logor (Int64.shift_left (Int64.of_int uid) pos) res in
        let pos = pos - 5 in
        let res = Int64.logor (Int64.shift_left (Int64.of_int s) pos) res in
        let pos = pos - 5 in
        let res = Int64.logor (Int64.shift_left (Int64.of_int num) pos) res in
        assert (pos = 32);
        Int64.logor v res
  
  let top = Interval(min_elt, max_elt)
  
  (* Tells when the value is top, i.e. the value is not known. *)
  (* If the value is symbolic, this function will return false, even if *)
  (* nothing is known about the value. *)
  let is_top = function
    | FSet _ -> false
    | Interval (l, h) -> l=min_elt && h=max_elt
  
  let fresh_symb_val () = encode_symb (get_fresh_uid ()) 0 0 0L
  
  let get_fresh_symb () = 
    FSet (NumSet.singleton (fresh_symb_val ())) 
  
  
  (* Check if one of the possible values is symbolic *)
  let is_symb_vals = function
    | FSet fs -> NumSet.exists is_symb fs
    | Interval _ -> false
  
  let interval_to_set l h = 
    let rec loop l h accum =
      if l>h then accum
      else loop (Int64.succ l) h (NumSet.add l accum) in
    loop l h NumSet.empty
    
  let set_to_interval s = 
    if is_symb_vals (FSet s) then top
    else Interval(NumSet.min_elt s, NumSet.max_elt s)
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

  let int64_to_bitstring x = 
    let res = List.fold_left (fun l i ->  
      let s = if Int64.logand (Int64.shift_right x i) 1L = 1L then "1" else "0" in
      s :: l
      ) [] (0 -- 64) in
    String.concat "" res
  let symb_to_string v = 
    (* assert (is_symb v); *)
    if is_symb v then begin 
      let uid = extract_enc_uid v in
      let start = extract_enc_start v in
      let num = extract_enc_num v in
      let known = String.sub (int64_to_bitstring (extract_enc_val v)) 32 32 in
      Bytes.fill known (31 - start) start 's';
      Bytes.fill known 0 (32 - start - num) 's';
      Printf.sprintf "S(uid = %x, known = [%d, %d), val = %s)" 
        uid start (start + num) known
    end else
      int64_to_bitstring v

  let pp_var_vals fmt avals = match avals with
  | FSet vals -> Format.fprintf fmt "@[{ ";
      NumSet.iter (fun v -> 
        if is_symb v then Format.fprintf fmt "%s " (symb_to_string v)
        else
          Format.fprintf fmt "%Ld @," v) vals;
      Format.fprintf fmt "}@]"
  | Interval(l,h) -> if is_top avals then Format.fprintf fmt "Top"
                     else Format.fprintf fmt "@[[%Ld, %Ld]@]" l h

  let var_vals_equal x1 x2 = match x1,x2 with
  | FSet s1, FSet s2 -> NumSet.equal s1 s2
  | Interval(l1,h1), Interval(l2,h2) -> l1=l2 && h1=h2
  | FSet s1, Interval(l2,h2) | Interval(l2,h2), FSet s1 ->
      NumSet.min_elt s1 = l2 && NumSet.max_elt s1 = h2 && 
      NumSet.cardinal s1 = Int64.to_int (Int64.sub h2 l2)
      (* NumSet.equal s1 (interval_to_set l2 h2) *)

  let print_one_var fmt v vals  = Format.fprintf fmt "@[%s in %a@]@;"
    (!variable_naming v) pp_var_vals vals

  let log_var v env= 
    let file = match !logFile with
      | None -> let f = (open_out "log.txt") in logFile := Some (f); f
      | Some f -> f
    in let log_var_vals avals  = match avals with
      | FSet vals -> Printf.fprintf file "{";
          NumSet.iter (fun v -> 
            if is_symb v then Printf.fprintf file "%s" (symb_to_string v)
            else
              Printf.fprintf file "%Ld " v) vals;
          Printf.fprintf file "}"
      | Interval(l,h) -> if is_top avals then Printf.fprintf file "Top"
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
    assert (numbits >= 0 && numbits <= 63);
    Int64.logand number (Int64.shift_right Int64.max_int (63 - numbits))
    (* match numbits with                           *)
    (*   | 32 -> Int64.logand number 0xFFFFFFFFL    *)
    (*   | 8 -> Int64.logand number 0xFFL           *)
    (*   | _ -> failwith "truncate: wrong argument" *)

  let set_map f set = NumSet.fold (fun x -> NumSet.add (f x)) set NumSet.empty
  
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
  let is_cf _ _ res =
    res < min_elt || res > max_elt
    
  let is_cf_shift sop original offset result =
    let carry = match sop with
      Shl | Rol -> Int64.logand result (Int64.shift_left Int64.one 32)
    | Shr | Sar | Ror ->
        let mask = Int64.shift_left Int64.one (Int64.to_int (Int64.pred offset)) in
        Int64.logand mask original
    in
    carry <> Int64.zero
  let is_zf res = ((truncate 32 res) = 0L)

  (* set_or_interval given two bounds either returns an interval or a set if its size is less than O.max_set_size *)
  let set_or_interval l h = if range_over (l,h) O.max_set_size then Interval(l,h) else FSet (interval_to_set l h)

  let init v2s = variable_naming:=v2s; VarMap.empty

  let new_var m v = m 
  (* VarMap.add v top m *)

  let delete_var m v = VarMap.remove v m
  
  let set_top = delete_var
  
  let is_var m vn = VarMap.mem vn m
  
  let upd_var env var values = if is_top values then 
      set_top env var
    else
      VarMap.add var values env
  
  let get_var m v = try (match VarMap.find v m with
    | FSet l -> Nt (NumSet.fold (fun vl env -> NumMap.add vl (VarMap.add v (FSet (NumSet.singleton vl)) m) env) l NumMap.empty)
    | Interval(l,h) -> if range_over (l,h) O.max_get_var_size then Tp
        else
          let rec f l h = if l>h then NumMap.empty
            else NumMap.add l (VarMap.add v (FSet (NumSet.singleton l)) m) 
                   (f (Int64.succ l) h) in
          Nt(f l h)
  ) with Not_found -> Tp
    (* failwith  (Printf.sprintf "valAD.get_var: non-existent variable %Lx\n" v) *)

  let set_var m v l h = VarMap.add v (set_or_interval l h) m
  
  let set_symbolic env var symbol =
    let svar = match symbol with 
    | None -> get_fresh_symb ()
    | Some s -> FSet (NumSet.singleton s) in
    let env = VarMap.add var svar env in
    let symb = 
    begin match svar with 
    | FSet s ->  
      assert (NumSet.cardinal s = 1);
      NumSet.choose s
    | _ -> assert false end; in
    env, symb

  let get_vals c m = match c with
    Cons cs -> FSet (NumSet.singleton cs)
      (* FSet (NumSet.singleton (truncate 32 cs)) *)
  | VarOp v -> try VarMap.find v m
               with Not_found -> top
              (* failwith (Printf.sprintf "valAD.get_vals: non-existent variable %Lx\n" v) *)

  let same_vars v cv = match cv with VarOp v2 -> v=v2 | Cons _ -> false

  (* lifts to interval representation if needed *)
  let lift_to_interval s = 
    if NumSet.cardinal s > O.max_set_size then set_to_interval s else FSet s
  
  (* return interval to set if small enough *)
  let unlift v = match v with 
  | Interval(l, h) ->  
    if Int64.sub h l <= Int64.of_int O.max_set_size then FSet (NumSet.of_list (range64 l (Int64.succ h))) else v
  | fs -> fs
  
  let interval_lub (l1,h1) (l2,h2) =
    (min l1 l2, max h1 h2)
  
  let interval_union (l1,h1) (l2,h2) =
    if h1 < l2 || h2 < l1 then (* disjoint intervals *)
      raise Bottom
    else interval_lub (l1,h1) (l2,h2)
    
  let interval_glb (l1,h1) (l2,h2) = 
    let l,h = (max l1 l2, min h1 h2) in
    if l > h then raise Bottom else (l,h) 
  
  let interval_diff (l1,h1) (l2,h2) =
    (* As intervals can represent only a couple of cases: *)
    if l1 = l2 && h1 > h2 then
      (h2, h1)
    else if h1 = h2 && l1 < l2 then
      (l1, l2)
    else 
      raise Bottom
  
  let change_uid v uid = 
    change_bits v (32 + 5 + 5) 20 (Int64.of_int uid)
  
  let var_join x y = match x,y with
  | FSet s1, FSet s2 -> let s = NumSet.union s1 s2 in
      if NumSet.cardinal s = 2 && (NumSet.for_all (fun v -> is_symb v) s) &&
        (* joining 2 symbolic values *)
        NumSet.cardinal (set_map (fun v -> change_uid v 0) s) = 1 then 
        (* if all is the same but the UID, then take a fresh UID *)
          let v = NumSet.choose s in 
          FSet (NumSet.singleton (change_uid v (get_fresh_uid ())))
      else
        lift_to_interval s 
  | Interval(l,h), FSet s | FSet s, Interval(l,h) ->
      Interval(min l (NumSet.min_elt s), max h (NumSet.max_elt s))
  | Interval (l1,h1), Interval (l2,h2) -> 
    let l,h = (interval_lub (l1,h1) (l2,h2)) in
    Interval (l,h)

  let join x y =
    let f k a b = match a,b with
        | Some c, Some d -> let newvar = var_join c d in
          if is_top newvar then None else Some newvar
        | Some _, None | None, Some _ -> Some top
        | _, _ -> assert false
    in
    VarMap.merge f x y
  
  let var_meet x y = match x,y with
  | FSet s1, FSet s2 -> let s = NumSet.inter s1 s2 in
      if NumSet.is_empty s then raise Bottom else FSet s
  | Interval(l,h), FSet s | FSet s, Interval(l,h) -> 
      let rs = NumSet.filter (fun x -> x>=l && x<=h) s in
      if NumSet.is_empty rs then raise Bottom else FSet rs
  | Interval(l1,h1), Interval(l2,h2) ->
      let l,h = interval_glb (l1,h1) (l2,h2) in
      if range_over (l,h) O.max_set_size then Interval(l,h)
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
      lift_to_interval s
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
  
  let extract_symb_properties value = (extract_enc_uid value), 
    (extract_enc_start value), (extract_enc_num value)
  
  (* Returns the UID and the distance of oldest known predecessor *)
  (* of this symbolic value. The distance is the difference between*)
  (* the predecesor's and the descendant's MSB *)
  let get_predecessor symb_val =
    let msb = (change_bits symb_val 0 32 0L) in
    match rev_find msb !predecessor_map with
    | Some (id, distance) -> (id, distance)
    | None -> (extract_enc_uid symb_val, 0) 
  (* Returns the UID of the descendant of symbolic value [sval] which*)
  (* has dinstance from it [distance] *)
  let get_descendant sval distance =
    let puid, current_dist = get_predecessor sval in
    let distance = distance + current_dist in
    let curr_id, start, num = extract_symb_properties sval in
    try
      let nval = TupleMap.find (puid, distance) !predecessor_map in
      let nid, nstart, nnum = extract_symb_properties nval in
      if (nstart, nnum) = (start, num) then nid
      else 
        (* We have a symbolic predecessor, where the symbolic are different. *)
        (* Currently we overapproximate this situaiton by taking a fresh symbol *)
        get_fresh_uid ()
    with Not_found ->
      let nid = get_fresh_uid () in
      predecessor_map := 
        TupleMap.add (puid, distance) (encode_symb nid start num 0L) !predecessor_map;
      nid 
  
  
  (* From a list of flag combinations, remove combinations not described by *)
  (* [cf_known] and [zf_known], where a value None means that *)
  (* the respective flag is unknown (either true or false) *)
  let remove_flags flist cf_known zf_known =
    List.fold_left (fun accum flgs -> 
      match cf_known with 
      | Some cf when cf <> flgs.cf -> accum 
      | _ -> match zf_known with
        | Some zf when zf <> flgs.zf -> accum
        | _ -> flgs :: accum
      ) [] flist
  
  (* Those functions check whether something about the flags is known, for *)
  (* symbolic values. A result None means that nothing is known *)
  (* (flag may be true or false). *)
  let symb_check_cf src dst res _ = 
    let uid, start, num = extract_symb_properties res in
    (* If the symbol hasn't changed and the MSB are symbolic, then there was *)
    (* no overflow *)
    if start + num < 32 (* the MSB are symbolic *) &&
     (is_symb src && uid = (extract_enc_uid src)) ||
     (is_symb dst && uid = (extract_enc_uid dst)) then Some false
    else
      None
      
  let symb_check_zf src dst res aop = 
    (* If the known part of the value is not zero, ZF is false *)
    if extract_enc_val res <> 0L then Some false
    else if aop = Some Sub && is_symb src && is_symb dst then
      (* in case of src SUB dst, we may know that src != dst => ZF is false *)
      (* e.g. if src = dst + c, where c is a non-zero constant *)
        let puid_src, distance_src = get_predecessor src in
        let puid_dst, distance_dst = get_predecessor dst in
        (* Common predecessor but different distance *)
        if puid_src = puid_dst && distance_src <> distance_dst then Some false
        else None
      else None
  
  let fmap_find_with_defval key defval fm = try(
    FlagMap.find key fm) with Not_found -> defval
  
  (* Returns three maps which hold possible (flags,values)-combinations *)
  (* for the destination, source variables and result *)
  (* Signaling the exact [aop] is a quick solution for being able to gain*)
  (* more precision on flag's values in case we have a symbolic result. *)
  let perform_op op dstset srcset cf_test zf_test aop = 
    let compute_op dst = NumSet.fold (fun src (dmap,smap,rmap) -> 
      let result = op dst src in
      let upd_val fmap flag_list value =
        let value = if is_symb value then value else truncate 32 value in
        List.fold_left (fun fmap flags -> 
          let vals = fmap_find_with_defval flags NumSet.empty fmap in
          FlagMap.add flags (NumSet.add value vals) fmap 
          ) fmap flag_list in
      let flag_list = 
        if is_symb result then 
          let fl = [ {cf = true; zf = true}; {cf = true; zf = false}; 
            {cf = false; zf = true}; {cf = false; zf = false} ] in
            let cf_known, zf_known = symb_check_cf src dst result aop,
               symb_check_zf src dst result aop in
            remove_flags fl cf_known zf_known
        else
          [{cf = cf_test dst src result; zf = zf_test result}] in
      let dstmap = upd_val dmap flag_list dst in
      let srcmap = upd_val smap flag_list src in
      let resmap = upd_val rmap flag_list result in
      (dstmap, srcmap,resmap)
    ) srcset (FlagMap.empty,FlagMap.empty,FlagMap.empty) in
    let dstmap,srcmap,resmap = NumSet.fold (fun dst (new_d,new_s,new_r) -> 
      let dstvals, srcvals, resvals = compute_op dst in
      fmap_combine dstvals new_d NumSet.union,
      fmap_combine srcvals new_s NumSet.union,
      fmap_combine resvals new_r NumSet.union
      ) dstset (FlagMap.empty,FlagMap.empty,FlagMap.empty) in
    let dstmap = FlagMap.map (fun nums -> lift_to_interval nums) dstmap in
    let srcmap = FlagMap.map (fun nums -> lift_to_interval nums) srcmap in
    let resmap = FlagMap.map (fun nums -> lift_to_interval nums) resmap in
    dstmap, srcmap,resmap
  
  (* Functions used by update_val *)
  let arithop_to_int64op = function
    | Add -> Int64.add
    | Addc -> Int64.add
    | And -> Int64.logand
    | CmpOp -> assert false (* should be handled by test_cmp *)
    | Or -> Int64.logor
    | Sub -> Int64.sub
    | Subb -> Int64.sub
    | Xor -> Int64.logxor


  let make_set_from_list = List.fold_left (fun s x -> NumSet.add x s) NumSet.empty
  
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

  let to_interval = function 
    | FSet s -> (set_to_interval s)
    | i -> i
  
  (* interval_arith takes a flg : position = {TT,...,FF}, an arith_op aop,
   * the Int64 operation oper and two intervals *)
  let interval_arith env aop oper dstvar dst_vals src_vals = 
    if is_symb_vals dst_vals || is_symb_vals src_vals then 
      raise Is_Top
    else let dst_vals, src_vals = (to_interval dst_vals), (to_interval src_vals) in
    let retmap = FlagMap.empty in
    (* Interval after operation *)
    let inter = match aop with 
    | Add | Addc | Sub | Subb -> interval_operation oper dst_vals src_vals 
    | And | Or | Xor -> begin match aop with 
        | And -> interval_and dst_vals src_vals
        | Or -> interval_or dst_vals src_vals
        | Xor -> (* a xor b = (a & not b) or (b & not a) *)
          let f a b = interval_and a (interval_not b) in
          interval_or (f dst_vals src_vals) (f src_vals dst_vals) 
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
    
    (* Logical operations should be covered by one of the upper cases *)
    (match aop with 
    | And | Or | Xor -> assert (not (FlagMap.is_empty retmap))
    | _ -> ());
    
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
    
    FlagMap.map (fun nums -> upd_var env dstvar nums) retmap
    

  let bool_to_int64 = function true -> 1L | false -> 0L
  
  (* Stores the new values of the dstvar (possibly updated from the computation)*)
  (* as well as the src_cvar values if the source is a variable.*)
  (* Both values are partitioned into the ones corresponding to any of the *)
  (* possible flag combinations *)
  let store_vals env dstvar dstmap src_cvar srcmap = 
    FlagMap.mapi (fun flgs dvals -> 
      let env = begin match src_cvar with
      | VarOp var -> VarMap.add var (FlagMap.find flgs srcmap) env
      | Cons _ -> env end in
      VarMap.add dstvar dvals env) dstmap
  
  (* Check if i lies within the interval [ start, start + num ) *)
  let in_interval i start num = i >= start && i < start + num 
  
  (* return true iff x[i] = 1 *)
  let get_bit i x = Int64.logand (Int64.shift_right x i) 1L = 1L
  
  
  exception Invalid_interval of int * int
  
  (* [symb_compute aop dst src] checks which changes will be made to the *)
  (* symbolic part of values and returns a function which *)
  (* performs those changes to [resval]. *)
  (* [resval] is the resulting value of the operation of the non-symbolic part*)
  (* (low 32 bits) of the two operands. *)
  let symb_compute aop dst src resval = 
    assert (is_symb dst || is_symb src);
    assert (not (is_symb resval));
    
    let symb_val, other_val = 
      if is_symb dst then dst,src else src,dst in
    
    let sval_uid, sval_start, sval_num = extract_symb_properties symb_val in
    let oval_uid, oval_start, oval_num = 
      if is_symb other_val then extract_symb_properties other_val
      else -1, 0, 32 in
    
    (* The possibility of carry and borrow when doing Add/Sub are handled *)
    (* conservatively: *)
    (*   if the known values are not the LSB, a new symbolic value *)
    (*   & forget known bits;*)
    (* and below: if there is carry or borrow, change the symbol without *)
    (*   forgetting the known bits.*)
    (* Can gain some precision by considering carry & borrow more carefully. *)
    match aop with
    | Add | Addc | Sub | Subb 
      when sval_start <> 0 || oval_start <> 0 -> 
        fresh_symb_val ()
    (* constant minus symbol *)
    | Sub | Subb
      when (is_symb dst) && not (is_symb src) -> 
        fresh_symb_val ()
    | _ -> 
      (* Bit-by-bit, apply changes to symbolic bits of the value (if needed). *)
      let (renew_uid, start, num, carry) = try
        List.fold_left (fun (renew_uid, start, num, carry) i ->
          let new_symbol = (true, start, num, carry) in
          let nsymb_sval_bit, nsymb_oval_bit = 
            in_interval i sval_start sval_num, in_interval i oval_start oval_num in
          let update_start start num i =
            if start + num = 0 then i
            else if i = start + num then start
            else (* Cannot represent the non-symbolic part as an interval *)
              raise (Invalid_interval (start, num)) in
          if nsymb_sval_bit && nsymb_oval_bit then
            (* i'th bit is not symbolic in both values, thus do nothing *)
            (renew_uid, update_start start num i, num + 1, carry)
          else if nsymb_sval_bit || nsymb_oval_bit then
            (* one bit is symbolic and the other not *)
            let concrete_bit = get_bit i (if nsymb_sval_bit then symb_val else other_val) in
            if (aop = And  && (not concrete_bit)) || (aop = Or && concrete_bit) then
              (* Learn new bit: if we do an AND with 0 or an OR with 1 *)
              (renew_uid, update_start start num i, num + 1, carry)
            else if ((aop = And  && concrete_bit) || (aop = Or && (not concrete_bit)))
              (* Symbolic bit is retained: AND with 1, OR with 0 *)
              || ((match aop with | Add | Sub | Xor -> true | _ -> false) &&
              not concrete_bit && (get_bit i resval) = false (* no carry/borrow *)
              ) then
                (* Symbolic bit is retained: ADD, SUB, XOR with 0 *)
                (renew_uid, start, num, carry)
            else if aop = Add && (get_bit i resval) (* add with a carry *) 
              && (i = sval_start + sval_num) then begin
              (true, start, num, true)
            end
            else
              (* Nothing known about those bits, take a new symbolic value *)
              new_symbol
          else if sval_uid = oval_uid (* the same symbolic bit *) then
            match aop with 
            | Sub | Xor 
              when not (get_bit i resval) (* be sure there is no borrow *) -> 
              (* Learn new bit: if we do a SUB or XOR with the same symbolic bits *)
              (renew_uid, update_start start num i, num + 1, carry) (* "updated" -> make this bit concrete *)
            | And | Or ->
              (* Symbolic bit is retained: AND or OR with the same symbolic value *)
              (renew_uid, start, num, carry)
            | _ -> new_symbol
          else
            (* a fresh symbolic part, the concrete part gets retained *)
            new_symbol
        ) (false, 0, 0, false) (0 -- 32) 
        with Invalid_interval (s,n) -> 
          (* Cannot represent further concrete bits; *)
          (* take a new symbol and keep the lower bits that are already known *)
          (true, s, n, false)
        in
      let uid = if not renew_uid then sval_uid
        else if carry  then begin 
          (* If there is a carry to the most-significant bits.*)
          (* make sure that for this (symbol, start, num) + carry there is *)
          (* only one new uid. *)
          (* If a symbolic value is derived by adding a constant to another*)
          (* symbolic value, store the original symbol and the new symbol's*)
          (* distance, i.e., the new MSB - the old MSB.*)
          (* For both the new and the old value, the symbolic bits should be*)
          (* at the same indices. *)
          get_descendant symb_val 1
        end else if is_symb symb_val && (not (is_symb other_val)) &&
          (aop = Add) && (get_bit 1 other_val = false) then
            (* The new value can be represented as s + c, where c is *)
            (* a positive constant.*)
            (* Make sure that only one such symbolic element exists. *)
            let distance = extract_enc other_val (start + num) (32 - start - num) in
            get_descendant symb_val (Int64.to_int distance)
        else
          get_fresh_uid () in
      (* nullify the unknown/symbolic bits *)
      let resval = change_bits resval 0 start 0L in
      let resval = change_bits resval (start + num) (32 - start - num) 0L in
      encode_symb uid start num resval
    
  
  let perform_one_arith aop cf x y = 
        (* If one of the values is symbolic, then *)
        (* (1) compute the operation on the non-symbolic part of the value*)
        (* (2) make changes to the symbolic part of the value; this may *)
        (*     entail learning the value of some symbolic bits (e.g. if we*)
        (*     have AND with 0-bits. *)
        (* Part (2) is performed by the following function: *)
        let symb_changes = if is_symb x || is_symb y then
            symb_compute aop x y
          else fun v -> v in
        (* clear the 32 most-significant bits of both operands *)
        let x,y = change_bits x 32 32 0L, change_bits y 32 32 0L in
        let y = match aop with 
        | Addc | Subb -> 
          (* Add carry flag value to operation *)
          Int64.add y (bool_to_int64 cf)
        | _ -> y in
        let res = arithop_to_int64op aop x y in
        symb_changes res
  
  (* Implements the effects of arithmetic operations *)
  let arith env flgs_init dstvar dst_vals srcvar src_vals aop = 
    if aop = Xor && (same_vars dstvar srcvar) then
      (*Then the result is 0 *)
      FlagMap.singleton {cf = false; zf = true} (VarMap.add dstvar zero env)
    else
      let oper = perform_one_arith aop flgs_init.cf in
      match dst_vals, src_vals with
      | FSet dset, FSet sset ->
        let _,srcmap,resmap = perform_op oper dset sset is_cf is_zf (Some aop) in
        store_vals env dstvar resmap srcvar srcmap
      | _, _ ->
      (* Convert sets into intervals in order to perform the operations;
         interval_arith may return either FSet or Interval *)
        try
          interval_arith env aop oper dstvar dst_vals src_vals
        with Is_Top ->
          (* let top_env = VarMap.add dstvar top env in *)
          let top_env = set_top env dstvar in
          let retmap = FlagMap.singleton {cf = false; zf = false} top_env in
          let retmap = FlagMap.add {cf = true; zf = false} top_env retmap in
          let retmap = FlagMap.add {cf = false; zf = true} top_env retmap in 
          FlagMap.add {cf = true; zf = true} top_env retmap
  
  let imul env flgs_init dstvar dst_vals srcvar src_vals aop arg3 =
    let imprecise_imul = 
      let retmap = FlagMap.singleton {cf = false; zf = false} 
        (set_top env dstvar) in
      FlagMap.add {cf = true; zf = false} (set_top env dstvar) retmap in
    if is_symb_vals dst_vals || is_symb_vals src_vals then imprecise_imul
    else match dst_vals, src_vals with
    | FSet dset, FSet sset ->
      (* zero flag is always not set *)
      let test_zf = (fun _ -> false) in
      (* carry flag is set when result has to be truncated *)
      let test_cf _ _ res =
        res < Int64.of_int32 Int32.min_int || res > Int64.of_int32 Int32.max_int in
      let srcmap,_,resmap = match arg3 with 
        | None -> (* 2 operands: do dst = dst * src *)
          perform_op Int64.mul dset sset test_cf test_zf None
        | Some imm -> (* 3 operands: do dst = src * imm*)
          let immset = NumSet.singleton imm in
          perform_op Int64.mul sset immset test_cf test_zf None in
        store_vals env dstvar resmap srcvar srcmap
    | _, _ -> 
      imprecise_imul
      
  
  (* interval_flag_test takes two intervals and a flag combination and returns
   * the corresponding intervals *)
  let interval_cmp env dstvar dvals srcvar svals =
    let dvals, svals = to_interval dvals, to_interval svals in
    (* The following corresponds to the flag setting of SUB DST SRC *)
    let dl,dh = get_bounds dvals in
    let sl,sh = get_bounds svals in
    (* Intersection between the two intervals *)
    let ndh = min dh (Int64.pred sh) in
    let nsl = max (Int64.succ dl) sl in
    let nsh = min (Int64.pred dh) sh in
    let ndl = max dl (Int64.succ sl) in
    let dstmap,srcmap = FlagMap.empty,FlagMap.empty in
    let dstmap,srcmap = if ndh < dl || nsl > sh then dstmap,srcmap 
    (* We should have [dl, min(dh,sh-1)] and [max(dl+1,sl), sh] *)
      else let flgs = {cf = true; zf = false} in
        FlagMap.add flgs (set_or_interval dl ndh) dstmap,
        FlagMap.add flgs (set_or_interval nsl sh) srcmap in
    let dstmap,srcmap = if ndl > dh || nsh < sl then dstmap,srcmap
      else let flgs = {cf = false; zf = false} in
        FlagMap.add flgs (set_or_interval ndl dh) dstmap,
        FlagMap.add flgs (set_or_interval sl nsh) srcmap in
    (* if the intersection of src and dst is not empty, then ZF can be set *)
    let meetZF = var_meet_ab dvals svals in
    match meetZF with
    | Bot -> dstmap,srcmap
    | Nb z -> let flgs = {cf = false; zf = true} in
      FlagMap.add flgs z dstmap, FlagMap.add flgs z srcmap

  (* perform TEST or CMP *)
  let test_cmp env flags fop dstvar dvals srcvar svals =
    let arith_op = match fop with
    | Atest -> And | Acmp -> Sub in
    let oper = perform_one_arith arith_op flags.cf in
    (* let oper = arithop_to_int64op arith_op in *)
    match dvals,svals with
    | FSet dset, FSet sset ->
        let dstmap,srcmap,_ = perform_op oper dset sset is_cf is_zf (Some arith_op) in
        store_vals env dstvar dstmap srcvar srcmap
    | _,_ ->
      if fop = Acmp then
        let dstmap,srcmap = interval_cmp env dstvar dvals srcvar svals in
        store_vals env dstvar dstmap srcvar srcmap
      else if srcvar = (VarOp dstvar) then
        (* Operation is "TEST X X". *)
        (* The result will be the same as "AND X X" because when dst = src,*)
        (*  AND does not change any values but only sets the flags *)
          interval_arith env And oper dstvar dvals svals 
        else
          failwith "general TEST instruction for intervals not implemented"

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
  let shift env flags sop dstvar vals offs_var off_vals mk = 
    let offsets = mask_vals mk off_vals in
    let oper = fun x y -> truncate 33 (shift_operator sop x y) in
      match vals, offsets with
      | FSet dset, FSet offs_set ->
          let cf_test = (is_cf_shift sop) in
          let _,srcmap,resmap = perform_op oper dset offs_set cf_test is_zf None in
          store_vals env dstvar resmap offs_var srcmap
      | Interval(l,h), FSet offs_set -> (* This doesn't work with rotate; return Top if rotate *)
        let newenv =
          let bound f b sup = NumSet.fold (fun offs r -> f (oper b offs) r) offs_set sup in
          let lb, ub = bound min l max_elt, bound max h min_elt in
          (* TODO : flag test *)
          (* Flags are not changed at all which is not correct! *)
          begin
            match sop with
              Ror | Rol -> set_top env dstvar
            | _ -> if ub < lb then env else VarMap.add dstvar (unlift(Interval(lb,ub))) env
          end in
        FlagMap.singleton flags newenv
      | _, _ ->  let fm = FlagMap.singleton {cf = true; zf = true} (set_top env dstvar) in
        let fm = FlagMap.add {cf = true; zf = false} (set_top env dstvar) fm in
        let fm = FlagMap.add {cf = false; zf = true} (set_top env dstvar) fm in
        FlagMap.add {cf = false; zf = false} (set_top env dstvar) fm

  (* Nullify bits corresponding to the mask *)
  let nullify_mask mask x = 
    Int64.logand (Int64.lognot mask) x
  
  (* Implements the effects of CDQ (copy double to quad): MSB written in EDX *)
  let cdq env flags dstvar dst_vals srcvar src_vals =
    let cdq_top = FSet (NumSet.add max_elt (NumSet.singleton 0L)) in
    let res_vals = cdq_top in
    (* For now: set result to top *)
    (* TODO: res_vals = if MSB of EAX known then MSB^32, else {0^32, 1^32} *)
    FlagMap.singleton flags (upd_var env dstvar res_vals)
    
    
    
  
  (* Implements the effects of MOV *)
  let mov env flags dstvar mkvar dst_vals srcvar mkcvar src_vals =
    let res_vals = match mkvar, mkcvar with
    (* 32 bits -> 32 bits MOV and 8 -> 32 : MOVZX *)
    | NoMask, NoMask -> src_vals
    | NoMask, msk ->
        if is_symb_vals src_vals then
          (* symbolic source in MOVZX *) 
          get_fresh_symb ()
        else
          mask_vals msk src_vals
    (* 8 -> 8 : MOVB *)
    | Mask mkv, Mask mkc ->
      if is_symb_vals dst_vals || is_symb_vals src_vals then
        (* 8-bit MOV with symbolic values *)
        get_fresh_symb ()
      else begin
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
            let varSet = set_map (fun x -> nullify_mask v_mask x) ds in
            (* Create a list of set combining all the posible values with the mask in place *)
            let doOp x = set_map (fun y -> Int64.logor x y) varSet in
            let setList = NumSet.fold (fun x r -> doOp x :: r) cvSet [] in
            (* Unite all the sets *)
            let finalSet = List.fold_left NumSet.union NumSet.empty setList in
            lift_to_interval finalSet
        | _, _ -> top
      end
    | _, _ -> failwith "ValAD.move: operation from 32 bits to 8 bits" in
  FlagMap.singleton flags (upd_var env dstvar res_vals)


  let update_val env flags dstvar mkvar srcvar mkcvar op arg3 = 
    let dvals = get_vals (VarOp dstvar) env in
    let svals = get_vals srcvar env in
    match op with
    | Amov -> mov env flags dstvar mkvar dvals srcvar mkcvar svals
    | Aarith aop -> if (mkvar, mkcvar) <> (NoMask,NoMask) then 
        failwith "ValAD: only 32-bit arithmetic is implemented"
      else arith env flags dstvar dvals srcvar svals aop
    | Ashift sop -> shift env flags sop dstvar dvals srcvar svals mkcvar
    | Aflag fop -> test_cmp env flags fop dstvar dvals srcvar svals
    | Aimul -> imul env flags dstvar dvals srcvar svals op arg3
    | Aneg -> (* NEG X is the same as 0 - X *)
      assert (same_vars dstvar srcvar);
      arith env flags dstvar (FSet (NumSet.singleton 0L)) srcvar svals Sub
    | Acdq -> 
      assert ((mkvar, mkcvar) = (NoMask,NoMask));
      cdq env flags dstvar dvals srcvar svals
      (* assert that src is EAX and dst is EDX *)
    | _ -> assert false
  
  let updval_set env flags dstvar mask cc = 
    let dvals = get_vals (VarOp dstvar) env in
    match dvals with
    | FSet dset -> 
      let msk = match mask with
      | NoMask -> assert false
      | Mask msk -> msk in
      let (v_mask, v_shift) = mask_to_intoff msk in
      let (tv, flg) = cc in
      begin match flg with
        | B | Z ->
          let flag = if flg == B then flags.cf else flags.zf in 
          let newval = bool_to_int64 (flag == tv) in
          (* shift the result according to the mask *)
          let newval = Int64.shift_left newval v_shift in
          (* clear the byte referred to by the mask and put the result there *)
          let val_set = set_map (fun x -> Int64.logor newval 
            (nullify_mask v_mask x)) dset in
          let new_env = VarMap.add dstvar (FSet val_set) env in 
          FlagMap.singleton flags new_env 
        | _ -> failwith "ValAD: SET with an unsupported condition"

      end
    | _ ->
      (* For SET with intervals we are imprecise and return top *)
      FlagMap.singleton flags (set_top env dstvar)

end
