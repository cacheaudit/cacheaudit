val instruction_addr_base : int64 ref
module SplitCacheArchitectureAD :
  functor (S : Signatures.STACK_ABSTRACT_DOMAIN) ->
    functor (IC : Signatures.CACHE_ABSTRACT_DOMAIN) ->
      Signatures.ARCHITECTURE_ABSTRACT_DOMAIN
module JointCacheArchitectureAD :
  functor (S : Signatures.STACK_ABSTRACT_DOMAIN) ->
    Signatures.ARCHITECTURE_ABSTRACT_DOMAIN
module NoInstructionCacheArchitectureAD :
  functor (S : Signatures.STACK_ABSTRACT_DOMAIN) ->
    Signatures.ARCHITECTURE_ABSTRACT_DOMAIN
