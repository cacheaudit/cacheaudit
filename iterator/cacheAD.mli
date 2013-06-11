(** Creates cache domain from given value domain *)
module Make :
  functor (SV : Signatures.SIMPLE_VALUE_AD) ->
    Signatures.CACHE_ABSTRACT_DOMAIN
