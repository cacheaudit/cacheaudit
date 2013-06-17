open Signatures
open X86Types

module type S = sig
  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
 (* val guard : t -> var_name -> guardop -> int64 -> t add_bottom *)
 (** Log the current value of a variable to the log file. For automated testing *)
  val log_var : var -> t -> unit
  val get_var : t -> var -> (t ValMap.t) add_top
 (* set_var env x l h sets the value of x to be in the interval [l,h] *)
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> varop -> 
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val is_var : t -> var -> bool
  val meet : t -> t -> t add_bottom (*TODO: should be add_bottom *)
  val flagop : t -> X86Types.arith_op -> cons_var -> cons_var -> 
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val shift : t -> X86Types.shift_op -> var -> cons_var -> mask -> 
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
end



module type VALADOPT =
  sig val max_get_var_size : int val max_set_size : int end
module ValADOptForMemory :
  sig val max_get_var_size : int val max_set_size : int end

module Make : functor (O : VALADOPT) -> S
