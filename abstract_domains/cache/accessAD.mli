(* Copyright (c) 2013-2014, IMDEA Software Institute.         *)
(* See ../../LICENSE for authorship and licensing information *)

(** Access abstract domain: keeps track of possible access sequences to sets in the cache. Can keep track of all accesses or only to changes by preventing stuttering *)

(** Whether stuttering should be prevented or not. Default is false. *)
val no_stuttering : bool ref

module Make :
  functor (C : CacheAD.S) -> CacheAD.S