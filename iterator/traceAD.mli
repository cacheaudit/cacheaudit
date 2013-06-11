(** Abstract domain maintaining a Trie-data structure
    where nodes store a Hit/Miss/Top-status of a cache access
 *)
module TraceAD :
  functor (CA : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.TRACE_ABSTRACT_DOMAIN
module NoTraceAD :
  functor (CA : Signatures.CACHE_ABSTRACT_DOMAIN) ->
    Signatures.TRACE_ABSTRACT_DOMAIN
