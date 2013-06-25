(** Parsing of ELF executables *)

exception NonElfFile

type t

val parse : AsmUtil.bits -> t

val read : string -> t

val sections : t -> ExecInterfaces.section list

val virtual_start : t -> int64
