(** An iterator module for analysis of executables *)

val unroll_count : int ref
val unroll_outer_loop : bool ref



module Make :
  functor (A : ArchitectureAD.S) ->
    sig
      val iterate :
        X86Headers.t -> 
        MemAD.mem_param ->
        Cfg.basicblock list ->
        CacheAD.cache_param ->
        CacheAD.cache_param option -> int64 -> unit
    end
