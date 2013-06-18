module type S = sig 
  type t
  val join: t -> t -> t
  val widen: t -> t -> t
  (* subseteq x y means gamma(x) in gamma(y) *)
  (* but false just means that we couldn't prove it *)
  val subseteq: t -> t -> bool
  val print : Format.formatter -> t -> unit
  (* To print traces *)
  val print_delta : t -> Format.formatter -> t -> unit
end
