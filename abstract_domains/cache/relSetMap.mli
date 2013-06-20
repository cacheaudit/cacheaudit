open NAD.DataStructures

module AFS :
  sig
    type t = AgeFunctionSet.AgeFunctionSet.t
    val combine : t -> t -> t
    val contradicts : t -> (var * int) list -> bool
    val empty : t
    val equal : t -> t -> bool
    val filter_comp :
      t -> var -> var -> (int -> int -> int) -> t
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

module type REL_SET_MAP =
  sig
    type t
    val init_with_max : (var -> string) -> int -> t
    val keys : t -> NumSet.t list
    val find : NumSet.t -> t -> AFS.t
    val add : NumSet.t -> AFS.t -> t -> t
    val filter : (NumSet.t -> AFS.t -> bool) -> t -> t
    val mapi : (NumSet.t -> AFS.t -> AFS.t) -> t -> t
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val differences : t -> t -> (NumSet.t * AFS.t * AFS.t) list
    val mem : NumSet.t -> t -> bool
    val for_all : (NumSet.t -> AFS.t -> bool) -> t -> bool
  end
module RelSetMap : REL_SET_MAP
