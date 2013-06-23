(** A module needed by [RelAgeAD] *)
open NAD.DataStructures

module type AGE_FUNCTION_SET =
  sig
    type t
    val combine : t -> t -> t
    val contradicts : t -> (var * int) list -> bool
    val empty : t
    val equal : t -> t -> bool
    val filter_comp : t -> var -> var -> (int -> int -> int) -> t
    val filter : t -> t -> t
    val inc_var : t -> var -> int -> t
    val is_empty : t -> bool
    val join : t -> t -> t
    val project : t -> var list -> t
    val singleton : var -> int -> t
    val subseteq : t -> t -> bool
    val toString : t -> string
    val values : t -> var -> int list
    val vset : t -> NumSet.t
  end
module RelAgeFnSet : AGE_FUNCTION_SET
