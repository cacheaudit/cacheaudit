(** Signatures of module interfaces *)

type ('a,'b) finite_set = Finite of ('a*'b) list | Top of 'b 

type 'a add_top = Nt of 'a | Tp
exception TopException

type 'a add_bottom = Nb of 'a | Bot
exception Bottom

let lift_combine f a1 a2 = match a1,a2 with
  Bot, x | x, Bot -> x
| Nb x1, Nb x2 -> Nb(f x1 x2)

type var = int64
(*let pp_var fmt = function n -> Format.fprintf fmt "V%Lx" n*)

type cons_var = Cons of int64 | VarOp of var

module ValSet = Set.Make(Int64)

(* Type of the abstract elements representing one variable *)
type var_t = FSet of ValSet.t | Interval of int64*int64

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

type cache_strategy = LRU | PLRU | FIFO (* PLRU stands for tree-based pseudo LRU *)

type cache_param = int * int * int * cache_strategy (* total size, line size, associativity. TODO use a record *)

module ValMap = Map.Make(Int64)




