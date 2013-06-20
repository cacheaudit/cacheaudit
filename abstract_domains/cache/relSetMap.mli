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
    val vset : t -> Signatures.ValSet.t
  end

module type REL_SET_MAP =
  sig
    type t
    val init_with_max : (var -> string) -> int -> t
    val keys : t -> Signatures.ValSet.t list
    val find : Signatures.ValSet.t -> t -> AFS.t
    val add : Signatures.ValSet.t -> AFS.t -> t -> t
    val filter : (Signatures.ValSet.t -> AFS.t -> bool) -> t -> t
    val mapi : (Signatures.ValSet.t -> AFS.t -> AFS.t) -> t -> t
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val differences : t -> t -> (Signatures.ValSet.t * AFS.t * AFS.t) list
    val mem : Signatures.ValSet.t -> t -> bool
    val for_all : (Signatures.ValSet.t -> AFS.t -> bool) -> t -> bool
  end
module RelSetMap : REL_SET_MAP
