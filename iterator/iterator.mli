val unroll_count : int ref
val unroll_outer_loop : bool ref



module Build :
  functor (A : Signatures.ARCHITECTURE_ABSTRACT_DOMAIN) ->
    sig
      val iterate :
        X86Headers.t ->
        (X86Types.reg32 * int64 * int64) list ->
        Cfg.basicblock list ->
        Signatures.cache_param ->
        Signatures.cache_param option -> int64 -> unit
    end
