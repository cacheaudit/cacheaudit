open Signatures

module MakeSeparate :
  functor (S : Signatures.STACK_ABSTRACT_DOMAIN) ->
    functor (IC : Signatures.CACHE_ABSTRACT_DOMAIN) -> ARCHITECTURE_ABSTRACT_DOMAIN
module MakeShared :
  functor (S : Signatures.STACK_ABSTRACT_DOMAIN) -> ARCHITECTURE_ABSTRACT_DOMAIN
module MakeDataOnly :
  functor (S : Signatures.STACK_ABSTRACT_DOMAIN) -> ARCHITECTURE_ABSTRACT_DOMAIN
