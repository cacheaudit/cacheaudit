open X86Types
open AbstractInstr
open AD.DataStructures

(** Architecture abstract domain: keeps track of the global state (currently:
    logical memory and caches), for use with an iterator *)

module type S =
sig
  include AD.S

  (** Creates an ArchitectureAD with the following parameters 
      {b TODO: Explain parameters. Can we use a record type for better readability?} *)
  val init: X86Headers.t -> 
    (((int64 * int64 * int64) list)*((reg32 * int64 * int64) list)) -> 
    CacheAD.cache_param -> CacheAD.cache_param option -> int64 -> t
    
  (** For an op32 expression, returns a finite list of possible
      values, each value associated with an approximation of the
      corresponding memory states leading to that particular value. In
      case no finite list can be determied, returns Top. {b TODO: Is
      "get_offset" really a descriptive name? Why offset?}  *)
  val get_offset: t -> op32 -> (int,t) finite_set
    
  (** Returns an overapproximation of the environments in which the condition holds,
      followed by an overapproximation of the environments in which it doesn't. *)
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
    
  (** Records a call and its effects on the stack. The first argument is the 
      address of the call, the second one is the return address. *)
  val call : t -> op32 -> int -> (int,t) finite_set 

  (** Records a return (and its effect on the stack). *)
  val return : t -> (int,t) finite_set
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val movzx : t -> op32 -> op8 -> t
  val load_address : t -> reg32 -> address -> t
  val flagop : t -> op32 flagop -> t
  val stackop : t -> stackop -> op32 -> t
  val shift : t -> shift_op -> op32 -> op8 -> t

    (** Records the addresses of operations, which is required for instruction caches *)
  val read_instruction: t -> int -> t

    (** Is used to signal from the iterator to the cache the
	time consumed by an instruction *)
  val elapse : t -> int -> t

end

(** Creates an ArchitectureAD with separate caches for data (represented by a StackAD) and instructions (represented by a CacheAD) *)
module MakeSeparate :
  functor (ST : StackAD.S) ->
    functor (IC : CacheAD.S) -> S

(** Creates an ArchitectureAD with shared cache for instruction and data *)
module MakeShared : functor (ST : StackAD.S) -> S

(** Creates an ArchitectureAD with data cache only *)
module MakeDataOnly : functor (ST : StackAD.S) -> S
