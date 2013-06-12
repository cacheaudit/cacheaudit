open Signatures

module type T =
  sig
    include Signatures.ABSTRACT_DOMAIN

    val init: X86Headers.t -> (X86Types.reg32 * int64 * int64) list -> cache_param -> cache_param option -> int64 -> t
  (* from a genop32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
    val get_offset: t -> op32 -> (int,t) finite_set
  (* test returns an overapproximation of the case where the condition is true
     followed by an overapproximation of the false case *)
    val test : t -> X86Types.condition -> (t add_bottom)*(t add_bottom)
  (* records a call (and its effect on the stack) The first argument is the 
     address of the call, the second one is the return address *)
    val call : t -> op32 -> int -> (int,t) finite_set 
    val return : t -> (int,t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> X86Types.reg32 -> X86Types.address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
  (* Used by trace recording abstract domains. elapse env d signals that time should be increased by d *)
    val elapse : t -> int -> t
    val read_instruction: t -> int -> t
  end


module MakeSeparate :
  functor (S : StackAD.T) ->
    functor (IC : Signatures.CACHE_ABSTRACT_DOMAIN) -> T
module MakeShared : functor (S : StackAD.T) -> T
module MakeDataOnly : functor (S : StackAD.T) -> T
