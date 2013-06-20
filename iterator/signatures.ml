(** Signatures of module interfaces *)


type varop = Op of X86Types.arith_op | Move

type cache_strategy = LRU | PLRU | FIFO (* PLRU stands for tree-based pseudo LRU *)

type cache_param = int * int * int * cache_strategy (* total size, line size, associativity. TODO use a record *)

module ValMap = Map.Make(Int64)




