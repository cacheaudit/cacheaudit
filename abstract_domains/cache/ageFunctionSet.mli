module type AGE_FUNCTION_SET =
  sig
    type t
    val combine : t -> t -> t
    val contradicts : t -> (Signatures.var * int) list -> bool
    val empty : t
    val equal : t -> t -> bool
    val filter_comp :
      t -> Signatures.var -> Signatures.var -> (int -> int -> int) -> t
    val filter : t -> t -> t
    val inc_var : t -> Signatures.var -> int -> t
    val is_empty : t -> bool
    val join : t -> t -> t
    val project : t -> Signatures.var list -> t
    val singleton : Signatures.var -> int -> t
    val subseteq : t -> t -> bool
    val toString : t -> string
    val values : t -> Signatures.var -> int list
    val vset : t -> Signatures.ValSet.t
  end
module AgeFunctionSet : AGE_FUNCTION_SET
