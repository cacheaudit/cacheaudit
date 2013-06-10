module type VALADOPT =
  sig val max_get_var_size : int val max_set_size : int end
module ValADOptForMemory :
  sig val max_get_var_size : int val max_set_size : int end
module OrdVar : sig type t = Signatures.var val compare : 'a -> 'a -> int end
module VarMap :
  sig
    type key = OrdVar.t
    type 'a t = 'a Map.Make(OrdVar).t
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
module ValSet :
  sig
    type elt = Int64.t
    type t = Set.Make(Int64).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end
module ValADFunctor :
  functor (O : VALADOPT) -> Signatures.VALUE_ABSTRACT_DOMAIN
module ValAD :
  sig
    type t = ValADFunctor(ValADOptForMemory).t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : (Signatures.var -> string) -> t
    val new_var : t -> Signatures.var -> t
    val delete_var : t -> Signatures.var -> t
    val get_var :
      t -> Signatures.var -> t Signatures.ValMap.t Signatures.add_top
    val set_var : t -> Signatures.var -> int64 -> int64 -> t
    val update_var :
      t ->
      Signatures.var ->
      Signatures.mask ->
      Signatures.cons_var ->
      Signatures.mask ->
      Signatures.varop ->
      t Signatures.add_bottom * t Signatures.add_bottom *
      t Signatures.add_bottom * t Signatures.add_bottom
    val is_var : t -> Signatures.var -> bool
    val meet : t -> t -> t Signatures.add_bottom
    val flagop :
      t ->
      X86Types.arith_op ->
      Signatures.cons_var ->
      Signatures.cons_var ->
      t Signatures.add_bottom * t Signatures.add_bottom *
      t Signatures.add_bottom * t Signatures.add_bottom
    val shift :
      t ->
      X86Types.shift_op ->
      Signatures.var ->
      Signatures.cons_var ->
      Signatures.mask ->
      t Signatures.add_bottom * t Signatures.add_bottom *
      t Signatures.add_bottom * t Signatures.add_bottom
  end
