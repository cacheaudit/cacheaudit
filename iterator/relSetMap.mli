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
module VarSetMap :
  sig
    type key = AgeFunction.VarSet.t
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
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
