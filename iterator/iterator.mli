val verbose : bool ref
val trace : bool ref
val unroll_count : int ref
val unroll_outer_loop : bool ref
val time_skip : int
val time_jmp : int
type 'a wto =
    Linear of Cfg.basicblock * 'a wto
  | SubWto of 'a iteration
  | EmptyWto
and 'a iteration = {
  head_invariant : 'a Signatures.add_bottom;
  head_block : Cfg.basicblock;
  inner_wto : 'a wto;
  next_wto : 'a wto;
  unroll_count : int;
}
module BlockMap :
  sig
    type key = Cfg.basicblock
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
val head_until : 'a -> 'a list -> 'a list
type graph_visit_tag = NotSeen | Visit of int | Visited
type 'a tarjan = {
  head : int;
  tags : graph_visit_tag BlockMap.t;
  counter : int;
  seen : Cfg.basicblock list;
  wto : 'a wto;
}
val visit :
  int ->
  graph_visit_tag BlockMap.t ->
  BlockMap.key list -> BlockMap.key -> 'a wto -> 'a tarjan
val component :
  int ->
  graph_visit_tag BlockMap.t ->
  BlockMap.key list -> BlockMap.key -> 'a tarjan
val tarjan : BlockMap.key list -> 'a wto
val dont_unroll_outer : 'a wto -> 'a wto
module Build :
  functor (A : Signatures.ARCHITECTURE_ABSTRACT_DOMAIN) ->
    sig
      val print : Format.formatter -> A.t BlockMap.t -> unit
      val widen : A.t Signatures.add_bottom -> A.t -> A.t
      val subseteq : A.t -> A.t Signatures.add_bottom -> bool
      val op32_one : Signatures.op32
      val read_and_interpret_instruction : A.t -> int * X86Types.instr -> A.t
      val find_out_edge : Cfg.basicblock list -> int -> Cfg.basicblock
      val out_invs :
        Cfg.basicblock list ->
        (int, 'a) Signatures.finite_set -> (Cfg.basicblock * 'a) list
      val interpret_block :
        A.t -> Cfg.basicblock -> (Cfg.basicblock * A.t) list
      type environment = A.t BlockMap.t
      val env_add : BlockMap.key -> A.t -> A.t BlockMap.t -> A.t BlockMap.t
      val env_remove :
        BlockMap.key list -> BlockMap.key -> 'a BlockMap.t -> 'a BlockMap.t
      val iterate :
        X86Headers.t ->
        (X86Types.reg32 * int64 * int64) list ->
        BlockMap.key list ->
        Signatures.cache_param ->
        Signatures.cache_param option -> int64 -> unit
    end
