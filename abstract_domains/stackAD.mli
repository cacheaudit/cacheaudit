open X86Types
open AbstractInstr
open AD.DataStructures

module type S = 
  sig
    include AD.S

    val init :
      X86Headers.t ->
        (((int64 * int64 * int64) list)*((reg32 * int64 * int64) list)) -> 
          CacheAD.cache_param -> t
    val get_offset : t -> op32 -> (int, t) finite_set
    val test : t -> condition -> t add_bottom * t add_bottom
    val call : t -> op32 -> int -> (int, t) finite_set
    val return : t -> (int, t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> reg32 -> address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> shift_op -> op32 -> op8 -> t


    (** [touch] is used to signal to the cache that a memory location has been accessed *)  
    val touch : t -> int64 -> t
      
    (** [elapse] is used to signal from the iterator to the cache the
	time consumed by an instruction *)
    val elapse : t -> int -> t
  end


module Make :
  functor (M : MemAD.S) -> S
