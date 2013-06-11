module RelCacheAD :
  functor (SV : Signatures.SIMPLE_REL_SET_DOMAIN) ->
    Signatures.CACHE_ABSTRACT_DOMAIN
module RelSetCacheAD :
  sig
    type t = RelCacheAD(SimpleRelSetAD.SimpleRelSetAD).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : Signatures.cache_param -> t
    val touch : t -> int64 -> t
    val touch_hm :
      t -> int64 -> t Signatures.add_bottom * t Signatures.add_bottom
    val elapse : t -> int -> t
    val count_cache_states : t -> Big_int.big_int
  end
