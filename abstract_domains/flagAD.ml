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

type flags_t = { cf : bool; zf : bool; }
module FlagMap = Map.Make(struct 
    type t = flags_t 
    let compare = Pervasives.compare 
  end)

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
  
  let fmap_to_old fmap =
    let get_values flgs = try( 
        Nb (FlagMap.find flgs fmap)
      ) with Not_found -> Bot in
    { tt = get_values {cf = true; zf = true};
      tf = get_values {cf = true; zf = false};
      ft = get_values {cf = false; zf = true};
      ff = get_values {cf = false; zf = false} }
  let old_to_fmap old = let fmap = FlagMap.empty in
    let set_vals_nobot flgs vals fmap = match vals with 
    | Bot -> fmap
    | Nb x -> FlagMap.add flgs x fmap in
    let fmap = set_vals_nobot {cf = true; zf = true} old.tt fmap in
    let fmap = set_vals_nobot {cf = true; zf = false} old.tf fmap in
    let fmap = set_vals_nobot {cf = false; zf = true} old.ft fmap in
    set_vals_nobot {cf = false; zf = false} old.ff fmap
  
  
  let is_bottom env = FlagMap.is_empty env

  let print_flag flgs fmt env = 
    Format.fprintf fmt "@[<2>When CF is %B and ZF is %B, @,%a@]"
              flgs.cf flgs.zf V.print env

  let print_delta_flag cf zf st1 fmt st2 = match st1,st2 with
    | Bot, Bot -> ()
    | Bot, Nb env -> begin match get_log_level FlagLL with
      | Debug -> Format.fprintf fmt "@[New case: CF %B, ZF %B: @,%a@]"
             cf zf V.print env 
      | _ -> ()
      end
    | Nb env, Bot -> begin match get_log_level FlagLL with
      | Debug -> Format.fprintf fmt "@[The case CF %B, ZF %B is no longer possible@]"
             cf zf 
      | _ -> ()
      end
    | Nb env1, Nb env2 -> begin match get_log_level FlagLL with
      | Debug -> if env1 != env2 then
             Format.fprintf fmt "@[Case CF %B, ZF %B: @,%a@]" cf zf
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
    let st1,st2 = fmap_to_old st1, fmap_to_old st2 in
    Format.fprintf fmt "@[%a @; %a @; %a @; %a@]"
       (print_delta_flag true true st1.tt) st2.tt
       (print_delta_flag true false st1.tf) st2.tf
       (print_delta_flag false true st1.ft) st2.ft
       (print_delta_flag false false st1.ff) st2.ff


(* Maps a function to the components of a state *) 
  let tmap f st = 
    { 
      tt = (match st.tt with
	| Nb x -> Nb (f x)
	| Bot -> Bot);
      tf = (match st.tf with
	| Nb x -> Nb (f x)
	| Bot -> Bot);
      ft = (match st.ft with
	| Nb x -> Nb (f x)
	| Bot -> Bot);
      ff = (match st.ff with
	| Nb x -> Nb (f x)
	| Bot -> Bot);
    }
      
(* Returns the join of the components of a state *)
  let localjoin st = 
    List.fold_left (fun x y ->
      match (x,y) with 
	| (Bot,Bot) -> Bot
	| (Nb a, Bot) -> Nb a
	| (Bot, Nb a) -> Nb a
	| (Nb a, Nb b) -> Nb (V.join a b)) Bot [st.tt;st.tf;st.ft;st.ff]

  let initial_flags = {cf = false; zf = false}
  (* Assumption: Initially no flag is set *)
  let init v2s = FlagMap.add initial_flags (V.init v2s) FlagMap.empty

let flmap_combine fm1 fm2 fn = FlagMap.merge (fun _ a b -> 
  match a,b with None,None -> None
  | Some x, None -> Some x | None, Some y -> Some y
  | Some x, Some y -> Some (V.join x y)) fm1 fm2

(* Component-wise join *)
  
  (* combine two flag maps, *)
  (* for keys present in only one of them return the respective values, *)
  (* if keys are defined in both, apply function [fn] to values *)
  let fmap_combine fm1 fm2 fn = FlagMap.merge (fun _ a b -> 
  match a,b with None,None -> None
  | Some x, None -> Some x | None, Some y -> Some y
  | Some x, Some y -> Some (fn x y)) fm1 fm2
  
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
    

  (* For operations that do not change flags (e.g. Mov) update_val treats states independently and joins after update.
     For operations that do change flags, update_val joins before the operations 
     Further precision could be gained by separately treating operations (Inc) that leave some flags untouched *)
 
  let update_val env var mkvar cvar mkcvar op = 
    let env = fmap_to_old env in
    let res = 
    match op with
    | Amov -> tmap (fun env -> 
        let tt,tf,ft,ff = V.update_val env var mkvar cvar mkcvar op in
        (* This is inefficient, but we assume all results are the same here *)
          assert (tt=tf && tf=ft && ft=ff); 
          match tt with Bot -> failwith "Bottom in update_val of falAD"
          | Nb x -> x) env
    | _ -> begin
        match localjoin env with
        | Bot -> raise Bottom
        | Nb x -> let wrap (a,b,c,d) =
          {tt = a; tf = b; ft = c; ff = d} in
          wrap (V.update_val x var mkvar cvar mkcvar op)
      end
    in old_to_fmap res


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

end
