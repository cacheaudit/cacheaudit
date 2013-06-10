module FlagAD :
  functor (V : Signatures.VALUE_ABSTRACT_DOMAIN) ->
    Signatures.FLAG_ABSTRACT_DOMAIN
module FlagsAD :
  sig
    type t = FlagAD(ValAD.ValAD).t
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
      Signatures.cons_var -> Signatures.mask -> Signatures.varop -> t
    val is_var : t -> Signatures.var -> bool
    val meet : t -> t -> t
    val test :
      t ->
      X86Types.condition -> t Signatures.add_bottom * t Signatures.add_bottom
    val flagop : t -> Signatures.cons_var Signatures.flagop -> t
    val shift :
      t ->
      X86Types.shift_op ->
      Signatures.var -> Signatures.cons_var -> Signatures.mask -> t
  end
