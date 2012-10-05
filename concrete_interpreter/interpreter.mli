open AsmUtil
open X86Types

val interpret : string -> X86Headers.t -> int -> (X86Types.reg32 * int64) list -> bool array -> unit

val get_genop32 : X86Headers.t -> genop32 -> int64
val set_genop32 : X86Headers.t -> genop32 -> int64 -> X86Headers.t

val get_genop8 : X86Headers.t -> genop8 -> int64
val set_genop8 : X86Headers.t -> genop8 -> int64 -> X86Headers.t
