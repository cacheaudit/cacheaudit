val min_frequency : int ref

module InstructionBasedAttacker :
  functor (C : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.CACHE_ABSTRACT_DOMAIN
module OneInstructionInterrupt :
  functor (C : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.CACHE_ABSTRACT_DOMAIN
module OneTimeInterrupt :
  functor (C : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.CACHE_ABSTRACT_DOMAIN
