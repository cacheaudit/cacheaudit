val verbose : bool ref
val precise_touch : bool ref
type adversay = Blurred | SharedSpace
val adversary : adversay ref
val plru_permut : int -> int -> int -> int
module CacheMap :
  sig
    type key = int
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
module AddrSet :
  sig
    type elt = Int64.t
    type t = Set.Make(Int64).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module CacheAD :
  functor (SV : Signatures.SIMPLE_VALUE_AD) ->
    Signatures.CACHE_ABSTRACT_DOMAIN
module SimpleCacheAD :
  sig
    type t = CacheAD(SimpleValAD.SimpleValAD).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : Signatures.cache_param -> t
    val touch : t -> int64 -> t
    val touch_hm :
      t -> int64 -> t Signatures.add_bottom * t Signatures.add_bottom
    val elapse : t -> int -> t
    val count_cache_states : t -> Big_int.big_int
  end
module IntervalCacheAD :
  sig
    type t = CacheAD(SimpleValAD.SimpleIntervalAD).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : Signatures.cache_param -> t
    val touch : t -> int64 -> t
    val touch_hm :
      t -> int64 -> t Signatures.add_bottom * t Signatures.add_bottom
    val elapse : t -> int -> t
    val count_cache_states : t -> Big_int.big_int
  end
module ProfSimpleCacheAD :
  sig
    type t =
        CacheAD(SimpleProfilingValAD.SimpleProfilingValAD(SimpleValAD.SimpleValAD)).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : Signatures.cache_param -> t
    val touch : t -> int64 -> t
    val touch_hm :
      t -> int64 -> t Signatures.add_bottom * t Signatures.add_bottom
    val elapse : t -> int -> t
    val count_cache_states : t -> Big_int.big_int
  end
module ProfRelSetCacheAD :
  sig
    type t =
        CacheAD(SimpleProfilingValAD.SimpleProfilingValAD(SimpleRelSetAD.SimpleRelSetAD)).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : Signatures.cache_param -> t
    val touch : t -> int64 -> t
    val touch_hm :
      t -> int64 -> t Signatures.add_bottom * t Signatures.add_bottom
    val elapse : t -> int -> t
    val count_cache_states : t -> Big_int.big_int
  end
