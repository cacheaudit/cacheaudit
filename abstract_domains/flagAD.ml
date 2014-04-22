open X86Types
open AbstrInstr
open AD.DS
open NumAD.DS
open Logger

module type S = 
  sig

  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
  val log_var : t -> var -> unit
  val get_var : t -> var -> (t NumMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val meet : t -> t -> t (*TODO: should be add_bottom *)
  val update_val : t -> var -> mask -> cons_var -> mask -> abstr_op -> t
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  end

module Make (V: ValAD.S) = struct
  
  (* Handles invariants corresponding to combinations of flags.
     For now only supports CF, ZF *)
  
  type t = V.t FlagMap.t
  
  (* for handling legacy functions *)
  type old_t = {
    tt : V.t add_bottom; (* CF true and ZF true *)
    tf : V.t add_bottom; (* CF true and ZF false *)
    ft : V.t add_bottom; (* CF false and ZF true *)
    ff : V.t add_bottom; (* CF false and ZF false *)
  }
  
  
  
  let is_bottom env = FlagMap.is_empty env

  let print_flag flgs fmt env = 
    Format.fprintf fmt "@[<2>When CF is %B and ZF is %B, @,%a@]"
              flgs.cf flgs.zf V.print env

  let print_delta_flag flgs st1 fmt st2 = match st1,st2 with
    | Bot, Bot -> ()
    | Bot, Nb env -> begin match get_log_level FlagLL with
      | Debug -> Format.fprintf fmt "@[New case: CF %B, ZF %B: @,%a@]"
             flgs.cf flgs.zf V.print env 
      | _ -> ()
      end
    | Nb env, Bot -> begin match get_log_level FlagLL with
      | Debug -> Format.fprintf fmt "@[The case CF %B, ZF %B is no longer possible@]"
             flgs.cf flgs.zf 
      | _ -> ()
      end
    | Nb env1, Nb env2 -> begin match get_log_level FlagLL with
      | Debug -> if env1 != env2 then
             Format.fprintf fmt "@[Case CF %B, ZF %B: @,%a@]" flgs.cf flgs.zf
             (V.print_delta env1) env2
      | _ -> Format.fprintf fmt "@[%a@]" (V.print_delta env1) env2
      end

  let print fmt st =
    if is_bottom st then Format.fprintf fmt "Flag domain is empty!@."
    else 
      Format.fprintf fmt "@[<hov 0>";
      FlagMap.iter (fun flgs vals -> print_flag flgs fmt vals) st;
      Format.fprintf fmt "@]"


  let print_delta st1 fmt st2 = 
    let get_values flgs fmap = try( 
          Nb (FlagMap.find flgs fmap)
        ) with Not_found -> Bot in
    let flgs_tt, flgs_tf, flgs_ft, flgs_ff = {cf = true; zf = true},
      {cf = true; zf = false}, {cf = false; zf = true}, {cf = false; zf = false} in
    Format.fprintf fmt "@[%a @; %a @; %a @; %a@]"
      (print_delta_flag flgs_tt (get_values flgs_tt st1)) (get_values flgs_tt st2)
      (print_delta_flag flgs_tf (get_values flgs_tf st1)) (get_values flgs_tf st2)
      (print_delta_flag flgs_ft (get_values flgs_ft st1)) (get_values flgs_ft st2)
      (print_delta_flag flgs_ff (get_values flgs_ff st1)) (get_values flgs_ff st2)
      
  
  let init v2s = FlagMap.add initial_flags (V.init v2s) FlagMap.empty

  let flmap_combine fm1 fm2 fn = FlagMap.merge (fun _ a b -> 
    match a,b with None,None -> None
    | Some x, None -> Some x | None, Some y -> Some y
    | Some x, Some y -> Some (V.join x y)) fm1 fm2

(* Component-wise join *)
  
  let join env1 env2 = fmap_combine env1 env2 V.join

(* Component-wise meet *)

  let meet env1 env2 = let res = 
    FlagMap.merge (fun _ a b -> 
        match a,b with 
        | _ , None | None, _ -> None
        | Some x, Some y -> 
          begin match V.meet x y with
          | Bot -> None
          | Nb nenv -> Some nenv
          end
      ) env1 env2 in
    if is_bottom res then raise Bottom
    else res
      

(* Component-wise widening. *)
 let widen env1 env2 = fmap_combine env1 env2 V.widen


  let new_var st var = FlagMap.map (fun x -> V.new_var x var) st

  let delete_var st var = FlagMap.map (fun x -> V.delete_var x var) st

  let set_var st var l h = FlagMap.map (fun x -> V.set_var x var l h) st
 
  let log_var env v = let _ = FlagMap.map (fun x -> V.log_var v x; x) env in ()

  
  exception Is_Top
  let get_var st var = try(
    let res = 
      FlagMap.fold (fun flgs vals f_nmap -> 
        let v_nmap = match V.get_var vals var with 
        | Tp -> raise Is_Top 
        | Nt v_nmap -> v_nmap in
        NumMap.fold (fun num num_venv f_nmap -> 
          let f_nmap = if (NumMap.mem num f_nmap) then f_nmap 
            else (NumMap.add num FlagMap.empty f_nmap) in
          let flgenv = NumMap.find num f_nmap in
          NumMap.add num (FlagMap.add flgs num_venv flgenv) f_nmap 
        ) v_nmap f_nmap
      ) st NumMap.empty in
    Nt res
  ) with Is_Top -> Tp
    
    

 let subseteq st1 st2 = 
  FlagMap.for_all (fun flgs vals -> 
    FlagMap.mem flgs st2 && V.subseteq vals (FlagMap.find flgs st2)) st1

  let test_bot st = if is_bottom st then Bot else Nb st

  let test st cond = 
    let part1,part2 = match cond with
    (* B <-> CF set *)
    | B -> FlagMap.partition (fun flgs _ -> flgs.cf) st
    (* BE <-> CF or ZF set *)
    | BE -> FlagMap.partition (fun flgs _ -> flgs.cf || flgs.zf) st
    (* Z <-> ZF set *)  
    | Z -> FlagMap.partition (fun flgs _ -> flgs.zf) st
    | _ -> failwith "Unsupported flag in test" in
    test_bot part1, test_bot part2


  (* For operations that do not change flags (e.g. Mov) update_val treats states independently and joins after update.
     For operations that do change flags, update_val joins before the operations 
     Further precision could be gained by separately treating operations (Inc) that leave some flags untouched *)
  let update_val env var mkvar cvar mkcvar op = 
    match op with
    | Amov -> FlagMap.mapi (fun flgs vals -> 
        let fopmap = V.update_val vals flgs var mkvar cvar mkcvar op in
          assert (FlagMap.cardinal fopmap = 1); 
          FlagMap.find flgs fopmap 
          ) env
    | _ -> let res =
        FlagMap.fold (fun flgs vals newmap -> 
            join newmap (V.update_val vals flgs var mkvar cvar mkcvar op)
          ) env FlagMap.empty in
        if is_bottom res then raise Bottom;
        res

end
