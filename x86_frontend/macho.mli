(* Copyright (c) 2013-2015, IMDEA Software Institute.          *)
(* See ../LICENSE for authorship and licensing information     *)

(** Parsing of MACH-O executables *)

exception NonMachOFile

type t

val parse : AsmUtil.bits -> t

val read : string -> t

val sections : t -> ExecInterfaces.section list

val virtual_start : t -> int64
