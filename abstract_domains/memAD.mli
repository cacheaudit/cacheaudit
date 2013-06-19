open Signatures
open X86Types
open AbstractInstr

module type S =
  sig
    include AD.S

  (* init is used to return an initial abstract state *)
  (* the first arguments returns the initial value at a given address if it *)
  (* is defined, None otherwize (meaning it's random *)
  val init: (int64 -> int64 option) -> (((int64 * int64 * int64) list)*((X86Types.reg32 * int64 * int64) list)) -> 
    cache_param -> t

  (* from a genop32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
  val get_offset: t -> op32 -> (int,t) finite_set
  val test : t -> X86Types.condition -> (t add_bottom)*(t add_bottom)
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val load_address : t -> X86Types.reg32 -> X86Types.address -> t
  val movzx : t -> op32 -> op8 -> t
  val flagop : t -> op32 flagop -> t
  val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
  (* Used by trace recording abstract domains. elapse env d signals that time should be increased by d *)
  val elapse : t -> int -> t
  val access_readonly : t -> int64 -> t
end
  

module Make :
  functor (F : FlagAD.S) ->
    functor (TR : TraceAD.S) -> S


(** Add an address to the list of logged addresses that will appear in the log file.
 * Log file values follow the order in which this function is called 
 * Whenever there is a call to print, all addresses are logged (usually twice) *)
val log_address: int64 -> unit
