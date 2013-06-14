(** The base type of all abstract domains used in CacheAudit *)

module type S = sig 
  type t

(** Join operator *)
  val join: t -> t -> t

(** Widening operator *)
  val widen: t -> t -> t


(** Test for inclusion. [subseteq x y] means that gamma(x) is
      contained in gamma(y); false just means that we could not prove it *)
  val subseteq: t -> t -> bool

(** Prints the current state *)
  val print : Format.formatter -> t -> unit

(** Prints the delta between two states. Needed for outputting execution traces *)
  val print_delta : t -> Format.formatter -> t -> unit
end
