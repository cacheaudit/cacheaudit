module SimpleVAD :
  functor (V : Signatures.VALUE_ABSTRACT_DOMAIN) ->
    Signatures.SIMPLE_VALUE_AD
module SimpleValAD :
  sig
    type t = SimpleVAD(ValAD.ValAD).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init_with_max : (Signatures.var -> string) -> int -> t
    val inc_var : t -> Signatures.var -> t
    val set_var : t -> Signatures.var -> int -> t
    val comp :
      t ->
      Signatures.var ->
      Signatures.var -> t Signatures.add_bottom * t Signatures.add_bottom
    val comp_with_val :
      t ->
      Signatures.var ->
      int -> t Signatures.add_bottom * t Signatures.add_bottom
    val exact_val : t -> Signatures.var -> int -> t Signatures.add_bottom
    val permute : t -> (int -> int) -> Signatures.var -> t
    val get_values : t -> Signatures.var -> int list
    val is_var : t -> Signatures.var -> bool
  end
module SimpleIntervalAD : Signatures.SIMPLE_VALUE_AD
