type cache_t
type hm_t

(* initialize an empty cache
   takes arguments cache_size (in bytes), line_size (in bytes) and associativity *)
val init : int -> int -> int -> cache_t

(* write an address in the cache *)
val write : cache_t -> int64 -> unit

(* update an address set, currently using LRU policy *)
val update_aset : 'a option list -> 'a option -> 'a option list

val set_verbose : cache_t -> bool -> unit
