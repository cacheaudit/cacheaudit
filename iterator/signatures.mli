type ('a, 'b) finite_set = Finite of ('a * 'b) list | Top of 'b
type 'a add_top = Nt of 'a | Tp
exception TopException
type 'a add_bottom = Nb of 'a | Bot
exception Bottom
val lift_combine :
  ('a -> 'a -> 'a) -> 'a add_bottom -> 'a add_bottom -> 'a add_bottom
type op32 = X86Types.genop32
type op8 = X86Types.genop8
type 'a flagop =
    ADcmp of 'a * 'a
  | ADtest of 'a * 'a
  | ADfset of X86Types.flag * bool
type memop = ADarith of X86Types.arith_op | ADmov | ADexchg
type stackop = ADpop | ADpush
type var = int64
val pp_var : Format.formatter -> int64 -> unit
type cons_var = Cons of int64 | VarOp of var
val var_to_consvar : var -> cons_var
val consvar_to_var : cons_var -> var
type mask_t = HH | HL | LH | LL
type mask = NoMask | Mask of mask_t
val rem_to_mask : int64 -> mask_t
val mask_to_intoff : mask_t -> int64 * int
type varop = Op of X86Types.arith_op | Move
type cache_strategy = LRU | PLRU | FIFO
type cache_param = int * int * int * cache_strategy
module ValMap :
  sig
    type key = Int64.t
    type 'a t = 'a Map.Make(Int64).t
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
module type ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
  end
module type STACK_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init :
      X86Headers.t ->
      (X86Types.reg32 * int64 * int64) list -> cache_param -> t
    val get_offset : t -> op32 -> (int, t) finite_set
    val test : t -> X86Types.condition -> t add_bottom * t add_bottom
    val call : t -> op32 -> int -> (int, t) finite_set
    val return : t -> (int, t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> X86Types.reg32 -> X86Types.address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
    val elapse : t -> int -> t
    val access_readonly : t -> int64 -> t
  end
module type ARCHITECTURE_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init :
      X86Headers.t ->
      (X86Types.reg32 * int64 * int64) list ->
      cache_param -> cache_param option -> int64 -> t
    val get_offset : t -> op32 -> (int, t) finite_set
    val test : t -> X86Types.condition -> t add_bottom * t add_bottom
    val call : t -> op32 -> int -> (int, t) finite_set
    val return : t -> (int, t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> X86Types.reg32 -> X86Types.address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
    val elapse : t -> int -> t
    val read_instruction : t -> int -> t
  end
module type MEMORY_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init :
      (int64 -> int64 option) ->
      (X86Types.reg32 * int64 * int64) list -> cache_param -> t
    val get_offset : t -> op32 -> (int, t) finite_set
    val test : t -> X86Types.condition -> t add_bottom * t add_bottom
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val load_address : t -> X86Types.reg32 -> X86Types.address -> t
    val movzx : t -> op32 -> op8 -> t
    val flagop : t -> op32 flagop -> t
    val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
    val elapse : t -> int -> t
    val access_readonly : t -> int64 -> t
  end
module type FLAG_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : (var -> string) -> t
    val new_var : t -> var -> t
    val delete_var : t -> var -> t
    val get_var : t -> var -> t ValMap.t add_top
    val set_var : t -> var -> int64 -> int64 -> t
    val update_var : t -> var -> mask -> cons_var -> mask -> varop -> t
    val is_var : t -> var -> bool
    val meet : t -> t -> t
    val test : t -> X86Types.condition -> t add_bottom * t add_bottom
    val flagop : t -> cons_var flagop -> t
    val shift : t -> X86Types.shift_op -> var -> cons_var -> mask -> t
  end
module type VALUE_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : (var -> string) -> t
    val new_var : t -> var -> t
    val delete_var : t -> var -> t
    val get_var : t -> var -> t ValMap.t add_top
    val set_var : t -> var -> int64 -> int64 -> t
    val update_var :
      t ->
      var ->
      mask ->
      cons_var ->
      mask ->
      varop -> t add_bottom * t add_bottom * t add_bottom * t add_bottom
    val is_var : t -> var -> bool
    val meet : t -> t -> t add_bottom
    val flagop :
      t ->
      X86Types.arith_op ->
      cons_var ->
      cons_var -> t add_bottom * t add_bottom * t add_bottom * t add_bottom
    val shift :
      t ->
      X86Types.shift_op ->
      var ->
      cons_var ->
      mask -> t add_bottom * t add_bottom * t add_bottom * t add_bottom
  end
module type SIMPLE_VALUE_AD =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init_with_max : (var -> string) -> int -> t
    val inc_var : t -> var -> t
    val set_var : t -> var -> int -> t
    val comp : t -> var -> var -> t add_bottom * t add_bottom
    val comp_with_val : t -> var -> int -> t add_bottom * t add_bottom
    val exact_val : t -> var -> int -> t add_bottom
    val permute : t -> (int -> int) -> var -> t
    val get_values : t -> var -> int list
    val is_var : t -> var -> bool
  end
module type SIMPLE_REL_SET_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init_with_max : (var -> string) -> int -> t
    val inc_var : t -> var -> t
    val set_var : t -> var -> int -> t
    val comp : t -> var -> var -> t add_bottom * t add_bottom
    val comp_with_val : t -> var -> int -> t add_bottom * t add_bottom
    val exact_val : t -> var -> int -> t add_bottom
    val permute : t -> (int -> int) -> var -> t
    val get_values : t -> var -> int list
    val is_var : t -> var -> bool
    val mem : t -> (var * int) list -> bool
    val partition : t -> var list list
  end
module type CACHE_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : cache_param -> t
    val touch : t -> int64 -> t
    val touch_hm : t -> int64 -> t add_bottom * t add_bottom
    val elapse : t -> int -> t
    val count_cache_states : t -> Big_int.big_int
  end
module type TRACE_ABSTRACT_DOMAIN =
  sig
    type t
    val join : t -> t -> t
    val widen : t -> t -> t
    val subseteq : t -> t -> bool
    val print : Format.formatter -> t -> unit
    val print_delta : t -> Format.formatter -> t -> unit
    val init : cache_param -> t
    val touch : t -> int64 -> t
    val elapse : t -> int -> t
  end
