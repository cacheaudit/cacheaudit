open X86Types
open AbstractInstr
open AD.DataStructures

(** Abstract domain that maps machine operations to operations on integer variables *)

module type S =
sig
  include AD.S
    
  (** Creates an MemAD with the following parameters {b TODO: Explain
      parameters. Can we use a record type for better readability?} *)
  val init: (int64 -> int64 option) -> 
    (((int64 * int64 * int64) list)*((X86Types.reg32 * int64 * int64) list)) -> 
    CacheAD.cache_param -> t
    
 (** For an op32 expression, returns a finite list of possible
      values, each value associated with an approximation of the
      corresponding memory states leading to that particular value. In
      case no finite list can be determied, returns Top. {b TODO: Is
      "get_offset" really a descriptive name? Why offset?}  *)
  val get_offset: t -> op32 -> (int,t) finite_set

  (** Returns an overapproximation of the environments in which the condition holds,
      followed by an overapproximation of the environments in which it doesn't. *)
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val load_address : t -> reg32 -> address -> t
  val movzx : t -> op32 -> op8 -> t
  val flagop : t -> op32 flagop -> t
  val shift : t -> shift_op -> op32 -> op8 -> t
    
  (** Is used to signal to the cache that a memory location has been accessed *)  
  val touch : t -> int64 -> t

  (** Is is used to signal from the iterator to the cache the time
      consumed by an instruction *)
  val elapse : t -> int -> t
end
    
(** Creates a memory abstract domain from a flag and cache abstract domains *)
module Make :
  functor (F : FlagAD.S) ->
    functor (C : CacheAD.S) -> S


(** Appends an address to the list of addresses that are logged.  The
    ordering of values in the log file corresponds to the ordering of
    that list.  Whenever there is a call to print, all addresses are
    logged (usually twice) {b TODO: Why twice?} *)
val log_address: int64 -> unit
