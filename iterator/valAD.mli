module type VALADOPT =
  sig val max_get_var_size : int val max_set_size : int end
module ValADOptForMemory :
  sig val max_get_var_size : int val max_set_size : int end

module ValADFunctor :
  functor (O : VALADOPT) -> Signatures.VALUE_ABSTRACT_DOMAIN

module ValAD :
  sig
    type t = ValADFunctor(ValADOptForMemory).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : (Signatures.var -> string) -> t
    val new_var : t -> Signatures.var -> t
    val delete_var : t -> Signatures.var -> t
    val get_var :
      t -> Signatures.var -> t Signatures.ValMap.t Signatures.add_top
    val set_var : t -> Signatures.var -> int64 -> int64 -> t
    val update_var :
      t ->
      Signatures.var ->
      Signatures.mask ->
      Signatures.cons_var ->
      Signatures.mask ->
      Signatures.varop ->
      t Signatures.add_bottom * t Signatures.add_bottom *
      t Signatures.add_bottom * t Signatures.add_bottom
    val is_var : t -> Signatures.var -> bool
    val meet : t -> t -> t Signatures.add_bottom
    val flagop :
      t ->
      X86Types.arith_op ->
      Signatures.cons_var ->
      Signatures.cons_var ->
      t Signatures.add_bottom * t Signatures.add_bottom *
      t Signatures.add_bottom * t Signatures.add_bottom
    val shift :
      t ->
      X86Types.shift_op ->
      Signatures.var ->
      Signatures.cons_var ->
      Signatures.mask ->
      t Signatures.add_bottom * t Signatures.add_bottom *
      t Signatures.add_bottom * t Signatures.add_bottom
  end
