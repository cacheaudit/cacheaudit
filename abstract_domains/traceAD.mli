open Signatures


(** Abstract domain maintaining a Trie-data structure
    where nodes store a Hit/Miss/Top-status of a cache access
 *)

module type S = sig
  include AD.S
  val init: cache_param -> t 
  
  val touch : t -> int64 -> t
  (* Used to keep track of time, if neccessary *)
  val elapse : t -> int -> t
end


module Make :
  functor (C : CacheAD.S) -> S
module MakeNot :
  functor (C : CacheAD.S) -> S
