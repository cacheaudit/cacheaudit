(** Signatures of module interfaces *)

type ('a,'b) finite_set = Finite of ('a*'b) list | Top of 'b 

type 'a add_top = Nt of 'a | Tp
exception TopException

type 'a add_bottom = Nb of 'a | Bot
exception Bottom

let lift_combine f a1 a2 = match a1,a2 with
  Bot, x | x, Bot -> x
| Nb x1, Nb x2 -> Nb(f x1 x2)

type op32 = X86Types.genop32
type op8 = X86Types.genop8

type 'a flagop = ADcmp of 'a*'a
            | ADtest of 'a*'a 
            | ADfset of X86Types.flag*bool

type memop = ADarith of X86Types.arith_op | ADmov | ADexchg

type stackop = ADpop | ADpush

type var = int64
let pp_var fmt = function n -> Format.fprintf fmt "V%Lx" n

type cons_var = Cons of int64 | VarOp of var

let var_to_consvar = function
  x -> VarOp x
let consvar_to_var = function
  | VarOp x -> x
  | Cons _ -> failwith "Signatures.consvar_to_var: can't convert constant"


type mask_t = HH | HL | LH | LL
type mask = NoMask | Mask of mask_t

let rem_to_mask = function
   0L -> HH
 | 1L -> HL
 | 2L -> LH
 | 3L -> LL
 | _ -> failwith "Signatures.rem_to_mask: incorrect offset"

let mask_to_intoff = function
   HH -> (0xFF000000L, 24)
 | HL -> (0x00FF0000L, 16)
 | LH -> (0x0000FF00L, 8)
 | LL -> (0x000000FFL, 0)

type varop = Op of X86Types.arith_op | Move

module ValMap = Map.Make(Int64)


(** The common type of all abstract domains *)
module type ABSTRACT_DOMAIN = sig
  type t
  val join: t -> t -> t
  val widen: t -> t -> t
  (* subseteq x y means gamma(x) in gamma(y) but false just means that we couldn't prove it *)
  val subseteq: t -> t -> bool
  val print : Format.formatter -> t -> unit
  (* To print traces *)
  val print_delta : t -> Format.formatter -> t -> unit
end

type cache_status_t = H | M | T

module type CALL_ABSTRACT_DOMAIN = sig
  include ABSTRACT_DOMAIN

  val init: X86Headers.t -> (X86Types.reg32 * int64 * int64) list -> (int * int * int) -> t

  (* from a genop32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
  val get_offset: t -> op32 -> (int,t) finite_set
  (* test returns an overapproximation of the case where the condition is true
     followed by an overapproximation of the false case *)
  val test : t -> X86Types.condition -> (t add_bottom)*(t add_bottom)
  (* records a call (and its effect on the stack) The first argument is the 
     address of the call, the second one is the return address *)
  val call : t -> op32 -> int -> (int,t) finite_set 
  val return : t -> (int,t) finite_set
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val movzx : t -> op32 -> op8 -> t
  val load_address : t -> X86Types.reg32 -> X86Types.address -> t
  val flagop : t -> op32 flagop -> t
  val stackop : t -> stackop -> op32 -> t
  val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
  
  val reset_cache_status: t -> t
  val get_cache_status : t -> cache_status_t option
end


module type MEMORY_ABSTRACT_DOMAIN = sig
  include ABSTRACT_DOMAIN
  
  (* init is used to return an initial abstract state *)
  (* the first arguments returns the initial value at a given address if it is defined, None otherwize (meaning it's random *)
  val init: (int64 -> int64 option) -> (X86Types.reg32 * int64 * int64) list -> (int * int * int) -> t

  (* from a genop32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
  val get_offset: t -> op32 -> (int,t) finite_set
  val test : t -> X86Types.condition -> (t add_bottom)*(t add_bottom)
  val memop : t -> memop -> op32 -> op32 -> t
  val memopb : t -> memop -> op8 -> op8 -> t
  val load_address : t -> X86Types.reg32 -> X86Types.address -> t
  val movzx : t -> op32 -> op8 -> t
  val flagop : t -> op32 flagop -> t
  val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
  
  val reset_cache_status: t -> t
  val get_cache_status : t -> cache_status_t option
end


module type FLAG_ABSTRACT_DOMAIN = sig
    (* Keeps track of flags. Currently restricted to combinations of
       CF and ZF) *)

  include ABSTRACT_DOMAIN
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
  val get_var : t -> var -> (t ValMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> varop -> t
  val is_var : t -> var -> bool
  val meet : t -> t -> t (*TODO: should be add_bottom *)
  val test : t -> X86Types.condition -> (t add_bottom)*(t add_bottom)
  val flagop : t -> cons_var flagop -> t
  val shift : t -> X86Types.shift_op -> var -> cons_var -> mask -> t
end


module type VALUE_ABSTRACT_DOMAIN = sig
  include ABSTRACT_DOMAIN
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
 (* val guard : t -> var_name -> guardop -> int64 -> t add_bottom *)
  val get_var : t -> var -> (t ValMap.t) add_top
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> varop -> (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val is_var : t -> var -> bool
  val meet : t -> t -> t add_bottom (*TODO: should be add_bottom *)
  val flagop : t -> X86Types.arith_op -> cons_var -> cons_var -> (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val shift : t -> X86Types.shift_op -> var -> cons_var -> mask -> (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
end

module type SIMPLE_VALUE_AD = sig
  include ABSTRACT_DOMAIN
  (* Initialize with a maximal value *)
  val init_with_max : (var->string) -> int -> t
  (* Increment variable, does not increase above the max value *)
  val inc_var : t -> var -> t
  (* Set variable to a value;
     if variable does not exist, create it *)
  val set_var : t -> var -> int -> t
  
  (* Add a variable to another variable or to a constant *)
  val add : t -> var -> int -> t
  
  (* Filter domain according to simple comparison of two variables x1 and x2*)
  (* the first result is approximates the cases when x1 < x2 and
     the second one when x1 > x2 *)
  val comp : t -> var -> var -> (t add_bottom)*(t add_bottom)
  val get_values : t -> var -> int list
  val is_var : t -> var -> bool
end


module type CACHE_ABSTRACT_DOMAIN = sig
  include ABSTRACT_DOMAIN
  (* initialize an empty cache
   takes arguments cache_size (in bytes), line_size (in bytes) and associativity *)
  val init : (int * int * int) -> t
  (* reads or writes an address into cache *)
  val touch : t -> int64 -> t
  val reset_status: t -> t
  val get_status: t -> cache_status_t option
end



module DummyCache = struct
  type t = unit
  let init _ _ _ = ()
  let join () () = ()
  let widen () () = ()
  let subseteq () () = true
  let write () i = ()
  let print fmt () = ()
  let print_delta () fmt () = ()
end



