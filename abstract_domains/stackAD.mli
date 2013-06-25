open X86Types
open AbstractInstr
open AD.DS

(** Stack abstract domain: keeps track of stack operations, 
    such as push and pop operations and function calls.  *)

module type S = 
sig
  include AD.S
    
    (** Creates a StackAD with the following parameters *)
  val init : X86Headers.t -> MemAD.mem_param -> CacheAD.cache_param -> t

  (** For an op32 expression, returns a finite list of possible
      values, each value associated with an approximation of the
      corresponding memory states leading to that particular value. In
      case no finite list can be determied, returns Top. {b TODO: Is
      "get_offset" really a descriptive name? Why offset?}  *)
  val get_offset : t -> op32 -> (int, t) finite_set

  (** Returns an overapproximation of the environments in which the condition holds,
      followed by an overapproximation of the environments in which it doesn't. *)
  val test : t -> condition -> t add_bottom * t add_bottom

  (** Records a call and its effects on the stack. The first argument is the 
      address of the call, the second one is the return address. *)
  val call : t -> op32 -> int -> (int, t) finite_set

  (** Records a return (and its effect on the stack). *)
  val return : t -> (int, t) finite_set
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val movzx : t -> op32 -> op8 -> t
  val load_address : t -> reg32 -> address -> t
  val flagop : t -> op32 flagop -> t
  val stackop : t -> stackop -> op32 -> t
  val shift : t -> shift_op -> op32 -> op8 -> t
    
  (** Is used to signal to the cache that a memory location has been accessed *)  
  val touch : t -> int64 -> t
      
  (** Is is used to signal from the iterator to the cache the
      time consumed by an instruction *)
  val elapse : t -> int -> t
end
  
(** Creates a StackAD from a MemAD *)
module Make :
  functor (M : MemAD.S) -> S
