(** Abstract domain maintaining a Trie-data structure
    where nodes store a Hit/Miss/Top-status of a cache access
 *)

module Make :
  functor (C : CacheAD.S) -> CacheAD.S
