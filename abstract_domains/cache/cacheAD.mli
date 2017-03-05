(* Copyright (c) 2013-2017, IMDEA Software Institute.             *)
(* See ../../LICENSE for authorship and licensing information     *)

(** Cache abstract domain: keeps track of CPU cache configurations *)

open AD.DS

type replacement_strategy = 
  | LRU  (** least-recently used *)
  | FIFO (** first in, first out *)
  | PLRU (** tree-based pseudo LRU *)


type cache_param_t = { 
  cs: int; (** cache size *)
  ls: int; (** line size *)
  ass: int; (** associativity *)
  str: replacement_strategy; (** strategy *)
  opt_precision: bool; (** optimize precision (may run slower) *)
  nb: int; (** number of bits from addresses invisible to adversaries *)
 } (** the cache parameters are given in bytes *) 

(** The signature of the cache abstract domain *)
module type S = sig
  include AD.S
  val init : cache_param_t -> t
  (** Initialize an empty cache according to the cache parameters defined 
      in [cache_param] *)
  val touch : t -> int64 -> NumAD.DS.rw_t -> t
  (** [touch addr] corresponds to a read or write of memory location [addr]. 
      The block containing [addr] is loaded into cache (if not in cache), and the
      positions of the elements in the corresponding cache set may be changed 
      according to the replacement strategy *)
  val touch_hm : t -> int64 -> NumAD.DS.rw_t -> (t add_bottom*t add_bottom)
  (** Same as [touch] but returns more precise information: 
      it returns a tuple [(hit_env,miss_env)], where the first element is an 
      (overapproximation of a) cache environment which results when there is
      a cache hit, and the second one is an environment corresponding to a 
      cache miss *)
  val elapse : t -> int -> t
  (** Used to keep track of time: [elapse x] informs that [x] clock cycles 
      have elapsed *)
  val count_cache_states : t -> Big_int.big_int
  (** Give the count of valid cache states *)
end


(** Creates a cache domain, given an age domain *)
module Make : functor (A : AgeAD.S) -> S

