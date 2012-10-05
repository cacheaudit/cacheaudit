open X86Types

type t

val empty : int -> t

val get_flag : t -> flag -> int
val set_flag : t -> flag -> int -> unit
