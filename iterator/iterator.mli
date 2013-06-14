val unroll_count : int ref
val unroll_outer_loop : bool ref



module Make :
  functor (A : ArchitectureAD.S) ->
    sig
      val iterate :
        X86Headers.t ->
        (X86Types.reg32 * int64 * int64) list ->
        Cfg.basicblock list ->
        Signatures.cache_param ->
        Signatures.cache_param option -> int64 -> unit
    end
