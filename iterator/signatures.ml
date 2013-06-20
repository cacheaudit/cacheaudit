(** Signatures of module interfaces *)


module ValSet = Set.Make(Int64)

(* Type of the abstract elements representing one variable *)
type var_t = FSet of ValSet.t | Interval of int64*int64

type varop = Op of X86Types.arith_op | Move

type cache_strategy = LRU | PLRU | FIFO (* PLRU stands for tree-based pseudo LRU *)

type cache_param = int * int * int * cache_strategy (* total size, line size, associativity. TODO use a record *)

module ValMap = Map.Make(Int64)




