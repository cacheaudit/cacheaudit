open X86Types
open AbstractInstr
open AD.DataStructures

module type S =
  sig
    include AD.S

  (** init is used to create an initial abstract state.
      The first arguments returns the initial value at a given address if it *)
  (* is defined, None otherwize (meaning it's random *)
  val init: (int64 -> int64 option) -> 
    (((int64 * int64 * int64) list)*((X86Types.reg32 * int64 * int64) list)) -> 
      CacheAD.cache_param -> t

  (* from a genop32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
  val get_offset: t -> op32 -> (int,t) finite_set
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val load_address : t -> reg32 -> address -> t
  val movzx : t -> op32 -> op8 -> t
  val flagop : t -> op32 flagop -> t
  val shift : t -> shift_op -> op32 -> op8 -> t

  (** [elapse] is used to signal from the iterator to the cache the
      time consumed by an instruction *)
  val elapse : t -> int -> t

  (** [touch] is used to signal to the cache that a memory location has been accessed *)  
  val touch : t -> int64 -> t
end
  
(** Creates a memory abstract domain from a flag and cache abstract domains *)
module Make :
  functor (F : FlagAD.S) ->
    functor (C : CacheAD.S) -> S


(** Add an address to the list of logged addresses that will appear in the log file.
 * Log file values follow the order in which this function is called 
 * Whenever there is a call to print, all addresses are logged (usually twice) *)
val log_address: int64 -> unit
