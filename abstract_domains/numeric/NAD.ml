(** The base type of numerci abstract domains used in CacheAudit *)
open Signatures
open X86Types
open AD.DataStructures

(** Module containing data structures common to numeric abstract domains *)
module DataStructures = struct
  type var = Int64.t
  (*let pp_var fmt = function n -> Format.fprintf fmt "V%Lx" n*)
  module VarMap = Map.Make(struct type t=var let compare = compare end)

  (** Data structure of arguments to numeric operations, which can only be either a variable or a numeric constant *)
  type cons_var = Cons of int64 | VarOp of var
  let var_to_consvar = function
    x -> VarOp x
  let consvar_to_var = function
    VarOp x -> x
  | Cons _ -> failwith "consvar_to_var: can't convert constant"

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

  module NumSet = Set.Make(Int64)

end

open DataStructures

module type S = sig 
  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t
 (* val guard : t -> var_name -> guardop -> int64 -> t add_bottom *)

 (** Log the current value of a variable to the log file. For automated testing *)
  val log_var : var -> t -> unit
  val get_var : t -> var -> (t ValMap.t) add_top

 (** set_var env x l h sets the value of x to be in the interval [l,h] *)
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> AbstractInstr.varop ->
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom) (*TODO tjis interface should be changed so that e.g. we have the flags in argument and return a tree *)
  val is_var : t -> var -> bool
  val meet : t -> t -> t add_bottom 
  val flagop : t -> arith_op -> cons_var -> cons_var ->
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val shift : t -> shift_op -> var -> cons_var -> mask ->
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
end

