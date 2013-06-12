val preset_address: int64 -> Signatures.var_t -> unit

module MemAD :
  functor (F : Signatures.FLAG_ABSTRACT_DOMAIN) ->
    functor (T : Signatures.TRACE_ABSTRACT_DOMAIN) ->
      Signatures.MEMORY_ABSTRACT_DOMAIN
