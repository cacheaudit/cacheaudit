module AFS :
  sig
    type t = AgeFunctionSet.AgeFunctionSet.t
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
    val vset : t -> AgeFunction.VarSet.t
  end

module type REL_SET_MAP =
  sig
    type t
    val init_with_max : (Signatures.var -> string) -> int -> t
    val keys : t -> AgeFunction.VarSet.t list
    val find : AgeFunction.VarSet.t -> t -> AFS.t
    val add : AgeFunction.VarSet.t -> AFS.t -> t -> t
    val filter : (AgeFunction.VarSet.t -> AFS.t -> bool) -> t -> t
    val mapi : (AgeFunction.VarSet.t -> AFS.t -> AFS.t) -> t -> t
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val differences : t -> t -> (AgeFunction.VarSet.t * AFS.t * AFS.t) list
    val mem : AgeFunction.VarSet.t -> t -> bool
    val for_all : (AgeFunction.VarSet.t -> AFS.t -> bool) -> t -> bool
  end
module RelSetMap : REL_SET_MAP
