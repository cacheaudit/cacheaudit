open Signatures
open X86Types

module type S = 
  sig
    include AD.S

    val init :
      X86Headers.t ->
      (X86Types.reg32 * int64 * int64) list -> cache_param -> t
    val get_offset : t -> op32 -> (int, t) finite_set
    val test : t -> X86Types.condition -> t add_bottom * t add_bottom
    val call : t -> op32 -> int -> (int, t) finite_set
    val return : t -> (int, t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> X86Types.reg32 -> X86Types.address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
    val elapse : t -> int -> t
    val access_readonly : t -> int64 -> t
  end


module Make :
  functor (M : MemAD.S) -> S
