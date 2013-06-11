
module TraceAD :
  functor (CA : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.TRACE_ABSTRACT_DOMAIN
module NoTraceAD :
  functor (CA : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.TRACE_ABSTRACT_DOMAIN
