open Signatures
open X86Types
open AbstractInstr
open AD.DataStructures

module type S =
  sig
    include AD.S

    val init: X86Headers.t -> (((int64 * int64 * int64) list)*((reg32 * int64 * int64) list)) -> cache_param -> cache_param option -> int64 -> t
  (* from an op32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
    val get_offset: t -> op32 -> (int,t) finite_set
  (* test returns an overapproximation of the case where the condition is true
     followed by an overapproximation of the false case *)
    val test : t -> condition -> (t add_bottom)*(t add_bottom)
  (* records a call (and its effect on the stack) The first argument is the 
     address of the call, the second one is the return address *)
    val call : t -> op32 -> int -> (int,t) finite_set 
    val return : t -> (int,t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> reg32 -> address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> shift_op -> op32 -> op8 -> t
  (* Used by trace recording abstract domains. elapse env d signals that time should be increased by d *)
    val elapse : t -> int -> t
    val read_instruction: t -> int -> t
  end


module MakeSeparate :
  functor (ST : StackAD.S) ->
    functor (IC : CacheAD.S) -> S
module MakeShared : functor (ST : StackAD.S) -> S
module MakeDataOnly : functor (ST : StackAD.S) -> S
