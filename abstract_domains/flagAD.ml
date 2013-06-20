open X86Types
open Signatures
open AbstractInstr
open AD.DataStructures
open NAD.DataStructures

module type S = 
  sig

  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
 (** Log the current value of a variable to the log file. For automated testing *)
  val log_var : t -> var -> unit
  val get_var : t -> var -> (t ValMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> varop -> t
  val is_var : t -> var -> bool
  val meet : t -> t -> t (*TODO: should be add_bottom *)
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  val flagop : t -> cons_var flagop -> t
  val shift : t -> shift_op -> var -> cons_var -> mask -> t
  end


module Make (V: NAD.S) = struct
  
  (* Handles invariants corresponding to combinations of flags.
     For now only supports CF, ZF *)

  type t = { 
    tt : V.t add_bottom; (* CF true and ZF true *)
    tf : V.t add_bottom; (* CF true and ZF false *)
    ft : V.t add_bottom; (* CF false and ZF true *)
    ff : V.t add_bottom; (* CF false and ZF false *)
  }
     
  let is_bottom st = st.tt=Bot && st.tf=Bot && st.ft=Bot && st.ff=Bot

  let print_flag cf zf fmt = function 
    Bot -> ()
  | Nb env -> Format.fprintf fmt "@[<2>When CF is %B and ZF is %B, @,%a@]" cf zf
                V.print env

  let print_delta_flag cf zf st1 fmt st2 = match st1,st2 with
    Bot, Bot -> ()
  | Bot, Nb env -> Format.fprintf fmt "@[New case: CF %B, ZF %B: @,%a@]"
                    cf zf V.print env
  | Nb env, Bot -> Format.fprintf fmt "@[The case CF %B, ZF %B is no longer possible@]"
                    cf zf
  | Nb env1, Nb env2 ->
      if env1 != env2 then
      Format.fprintf fmt "@[Case CF %B, ZF %B: @,%a@]" cf zf
        (V.print_delta env1) env2

  let print fmt st =
    if is_bottom st then Format.fprintf fmt "Flag domain is empty!@."
    else Format.fprintf fmt "@[%a @; %a @; %a @; %a@]@;"
       (print_flag true true) st.tt
       (print_flag true false) st.tf
       (print_flag false true) st.ft
       (print_flag false false) st.ff

  let print_delta st1 fmt st2 = 
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

  type position = TT | TF | FT | FF

  let one_flag pos nenv = match pos with
    TT -> { tt = Nb nenv; tf = Bot; ft = Bot; ff = Bot }
  | TF -> { tf = Nb nenv; tt = Bot; ft = Bot; ff = Bot }
  | FT -> { ft = Nb nenv; tf = Bot; tt = Bot; ff = Bot }
  | FF -> { ff = Nb nenv; tf = Bot; ft = Bot; tt = Bot }

  let set_pos st pos env = match pos with
    TT -> {st with tt=Nb env}
  | TF -> {st with tf=Nb env}
  | FT -> {st with ft=Nb env}
  | FF -> {st with ff=Nb env}
      
  (* Assumption: Initially no flag is set *)
  let init v2s = {
    tt = Bot;
    tf = Bot;
    ft = Bot;
    ff = Nb (V.init v2s)
  }	       
          
  let wrap (a,b,c,d) =
    {tt = a; tf = b; ft = c; ff = d}




(* Component-wise join *)

  let join st1 st2 = 
    let compjoin c1 c2 = match (c1,c2) with
      | (Bot,Bot) -> Bot
      | (Nb a, Bot) -> Nb a
      | (Bot, Nb b) -> Nb b
      | (Nb a, Nb b) -> Nb (V.join a b)
    in { 
      tt=compjoin st1.tt st2.tt;
      tf=compjoin st1.tf st2.tf;
      ft=compjoin st1.ft st2.ft;
      ff=compjoin st1.ff st2.ff;}

(* Component-wise meet *)

  let meet st1 st2 = 
    let compmeet c1 c2 = match (c1,c2) with
      | (Bot,Bot) -> Bot
      | (Nb a, Bot) -> Bot
      | (Bot, Nb b) -> Bot
      | (Nb a, Nb b) -> V.meet a b
    in { 
      tt=compmeet st1.tt st2.tt;
      tf=compmeet st1.tf st2.tf;
      ft=compmeet st1.ft st2.ft;
      ff=compmeet st1.ff st2.ff;}
      

(* Component-wise widening. *)
 let widen st1 st2 = 
    let compwiden c1 c2 = match (c1,c2) with
      | (Bot,Bot) -> Bot
      | (Nb a, Bot) -> Nb a
      | (Bot, Nb b) -> Nb b
      | (Nb a, Nb b) -> Nb (V.widen a b)
    in { 
      tt=compwiden st1.tt st2.tt;
      tf=compwiden st1.tf st2.tf;
      ft=compwiden st1.ft st2.ft;
      ff=compwiden st1.ff st2.ff;}


  let new_var st var = tmap (fun x -> V.new_var x var) st

  let delete_var st var = tmap (fun x -> V.delete_var x var) st

  let set_var st var l h = tmap (fun x -> V.set_var x var l h) st
 
  let log_var env v = let _ = tmap (fun x -> V.log_var v x; x) env in ()

(* get_var : FlagAD.t -> var -> EnvMap add_top *)

  let get_var st var = 
    (* from a possibly bottom component, the set of values *)
    let extract comp = match comp with
      Bot -> Bot
    | Nb env -> Nb(V.get_var env var) in
    let lift pos venv = match venv with
      Bot -> Bot
    | Nb Tp -> Nb Tp
    | Nb(Nt vmap) -> Nb(Nt(ValMap.map (one_flag pos) vmap)) in
    let merge fenv venv pos = match fenv, venv with
      _, Bot -> fenv
    | Bot, _ -> lift pos venv
    | Nb Tp, _ | _, Nb Tp -> Nb Tp
    | Nb(Nt fenv), Nb(Nt venv) -> 
        Nb(Nt(ValMap.merge (fun _ fe ve -> match fe, ve with
          Some st, Some v -> Some(set_pos st pos v)
        | Some st, None -> Some st
        | None, Some v -> Some(one_flag pos v) 
        | None, None -> None
        ) fenv venv)) in
    match merge(merge (merge (lift TT (extract st.tt)) (extract st.tf) TF)
                      (extract st.ft) FT) (extract st.ff) FF with
      Bot -> failwith "Bottom in FladAD.get_var"
    | Nb x -> x
(*
    assert(not(is_bottom st));
    let bot = {tt=Bot;tf=Bot;ft=Bot;ff=Bot} in
    let extract comp = match comp with 
      | Bot ->  ValMap.empty
      | Nb env -> (match (V.get_var env var) with
	      | Tp  -> failwith "flagAD: get_var can't handle top"
	      | Nt x -> assert(not(ValMap.is_empty x));x)
    in
    (* Merge components 1 and 2 *)
    let merger12 _ a b = match (a,b) with
      | (Some x, Some y) -> Some {bot with tt=Nb x; tf=Nb y}
      | (Some x, None) -> Some {bot with tt= Nb x}
      | (None, Some y) -> Some {bot with tf = Nb y}
      | (None, None) -> None 
    in
    (* Merge components 2 and 3 *)
    let merger23 _ a b = match (a,b) with
      | (Some x, Some y) -> Some {x with ft = Nb y}
      | (Some x, None) -> Some x
      | (None, Some y) -> Some {bot with ft = Nb y}
      | (None, None) -> None  
    in
    (* Merge components 3 and 4 *)
    let merger34 _ a b = match (a,b) with 
      | (Some x, Some y) -> Some {x with tt = Nb y}
      | (Some x, None) -> Some x
      | (None, Some y) -> Some {bot with tt = Nb y}
      | (None, None) -> None  
    in Nt (ValMap.merge merger34 
    (ValMap.merge merger23
       (ValMap.merge merger12 (extract st.tt) (extract st.tf))
       (extract st.ft))
    (extract st.tt))
*)

  (* For operations that do not change flags (e.g. Mov) update_var treats states independently and joins after update.
     For operations that do change flags, update_var joins before the operations 
     Further precision could be gained by separately treating operations (Inc) that leave some flags untouched *)
 
  let update_var st var mkvar cvar mkcvar varop = 
    match varop with
      | Move -> tmap (fun env -> 
          let tt,tf,ft,ff = V.update_var env var mkvar cvar mkcvar varop in
(* This is inneffficient en should be redisgned, but we assume all results are the same here *)
          assert (tt==tf && tf==ft && ft==ff); 
          match tt with Bot -> failwith "Bottom in update_var of falAD"
          | Nb x -> x) st
      | Op _ -> (
	match localjoin st with
	  | Bot -> raise Bottom
	  | Nb x -> wrap (V.update_var x var mkvar cvar mkcvar varop))

	      
    
(* is_var returns true iff variable exists in *all* non-Bottom value domains *)
(* Makes sense? *)
 
  let is_var st name = 
    List.fold_left 
      (fun x y -> match y with
	| Bot -> true
	| Nb a -> (x && V.is_var a name))
      true [st.tt;st.tf;st.ft;st.ff]

      

 let subseteq st1 st2 = 
   let compsubseteq c1 c2 = match (c1,c2) with
   | (Bot,Bot) -> true
   | (Nb a, Bot) -> false
   | (Bot, Nb b) -> true
   | (Nb a, Nb b) -> V.subseteq a b
   in (compsubseteq st1.tt st2.tt) && (compsubseteq st1.tf st2.tf) &&
   (compsubseteq st1.ft st2.ft) && (compsubseteq st1.ff st2.ff)


 let test_bot st = if is_bottom st then Bot else Nb st

 let test st cond = match cond with
   (* B <-> CF set *)
   | B -> 
     test_bot {st with ft=Bot; ff= Bot},   (* case: true *)
     test_bot {st with tt=Bot; tf= Bot}    (* case: false *)
   (* BE <-> CF or ZF set *)
   | BE ->
     test_bot {st with ff=Bot},
     test_bot {st with tt=Bot; tf=Bot; ft=Bot}
  (* Z <-> ZF set *)  
   | Z -> 
     test_bot {st with tf=Bot; ff=Bot},
     test_bot {st with tt = Bot; ft = Bot}
  | _ -> failwith "Unsupported flag in test"

    
 let flagop st fo =
   match localjoin st with
     Bot -> failwith "Bottom in falgAD.flagop"
   | Nb x -> (match fo with
       ADfset _  -> failwith "FlagAD.flagop: Setting of flags not yet supported"
     | ADtest(v1,v2) -> wrap (V.flagop x And v1 v2)
     | ADcmp(v1,v2) -> wrap (V.flagop x Sub v1 v2)
     )


  let shift st sop var cvar mk =
    match localjoin st with
    | Bot -> raise Bottom
    | Nb a -> wrap (V.shift a sop var cvar mk)


end
