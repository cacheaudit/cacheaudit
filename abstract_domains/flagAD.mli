open X86Types
open AbstractInstr
open AD.DS
open NumAD.DS

(** Flag abstract domain: keeps track of the relationship of flags and
    variable values. Currently restricted to combinations of CF and
    ZF) *)

module type S = 
sig
  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
  (** Log the current value of a variable to the log file. For automated testing *)
  val log_var : t -> var -> unit
  val get_var : t -> var -> (t NumMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> varop -> t
  val is_var : t -> var -> bool
  val meet : t -> t -> t (*TODO: should be add_bottom *)
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  val flagop : t -> cons_var flagop -> t
  val shift : t -> shift_op -> var -> cons_var -> mask -> t
  end

(** Creates a flag abstract domain from a numeric abstract domain *)
module Make :
  functor (V : NumAD.S) -> S
