(* Copyright (c) 2013-2017, IMDEA Software Institute.          *)
(* See ../LICENSE for authorship and licensing information     *)

(** Some shared functions *)

open Big_int
open NumAD.DS

(** Computes the logarithm to base 2 of a Bigint number,
the result is a floating-point number *)
val log2 : big_int -> float

(** Gives the product of a list of Int64's; the result is a big_int*)
val prod : int64 list -> big_int

(** [partition elts pfn] partitions the NumSet [elts] according to the 
    function [pfn]. The result is an IntMap wich maps integers to their
    images in [pfn] *)
val partition : NumSet.t -> (NumSet.elt -> int) -> NumSet.t IntMap.t

(** [i -- j] returns the list [\[i; i+1; ... ; j-1\]] *)
val (--) : int -> int -> int list
(** [range64 i j] works as i -- j for Int64 values *)
val range64 : int64 -> int64 -> int64 list

(**[reg_to_var r]  rturns the variable number that corresponds to register r *)
val reg_to_var: X86Types.reg32 -> int64

