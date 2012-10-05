open X86Types

type t

val empty : int -> t

val get_value_32 : t -> reg32 -> int64
val set_value_32 : t -> reg32 -> int64 -> unit

val get_value_16 : t -> reg16 -> int64
val set_value_16 : t -> reg16 -> int64 -> unit

val get_value_8 : t -> reg8 -> int64
val set_value_8 : t -> reg8 -> int64 -> unit

val print_regs : t -> unit

val get_segment : t -> segment_reg -> int64
val set_segment : t -> segment_reg -> int64 -> unit
