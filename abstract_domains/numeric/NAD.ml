(** The base type of the numeric abstract domains used in CacheAudit *)
open X86Types
open AD.DataStructures

(** Module containing data structures common to numeric abstract domains *)
module DataStructures = struct

  (** Type of variables *)
  type var = Int64.t
      
  (** Type for numeric operands, which can be either variables or numeric constant *)
  type cons_var = Cons of int64 | VarOp of var

  (** Converts cons_var to var *)
  let consvar_to_var = function (* TODO: Rename to operand_to_var? *)
    | VarOp x -> x
    | Cons _ -> failwith "consvar_to_var: can't convert constant"


  (** Types for masking operations *)
  type mask_t = HH | HL | LH | LL
  type mask = NoMask | Mask of mask_t

  let rem_to_mask = function
    | 0L -> HH
    | 1L -> HL
    | 2L -> LH
    | 3L -> LL
    | _ -> failwith "rem_to_mask: incorrect offset"

  let mask_to_intoff = function
    | HH -> (0xFF000000L, 24)
    | HL -> (0x00FF0000L, 16)
    | LH -> (0x0000FF00L, 8)
    | LL -> (0x000000FFL, 0)

  (**/**) (* Definitions below are not included in documentation *)

  module NumSet = Set.Make(Int64)
  module NumMap = Map.Make(Int64)
  module IntSet = Set.Make(struct type t = int let compare = compare end)
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  module VarMap = Map.Make(struct type t=var let compare = compare end)

end

open DataStructures

module type S = sig 
  include AD.S
  val init : (var->string) -> t
  val new_var : t -> var -> t
  val delete_var : t -> var -> t


 (** Log the current value of a variable to the log file. For automated testing *)
  val log_var : var -> t -> unit
  val get_var : t -> var -> (t NumMap.t) add_top

 (** set_var env x l h sets the value of x to be in the interval [l,h] *)
  val set_var : t -> var -> int64 -> int64 -> t
  val update_var : t -> var -> mask -> cons_var -> mask -> AbstractInstr.varop ->
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom) (*TODO this interface should be changed so that e.g. we have the flags in argument and return a tree *)
  val is_var : t -> var -> bool
  val meet : t -> t -> t add_bottom 
  val flagop : t -> arith_op -> cons_var -> cons_var ->
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val shift : t -> shift_op -> var -> cons_var -> mask ->
    (t add_bottom)*(t add_bottom)*(t add_bottom)*(t add_bottom)
  val var_names : t -> NumSet.t
end

