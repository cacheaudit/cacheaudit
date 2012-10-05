open X86Types

type t

val init : int -> t

val get_flag : t -> flag -> int
val set_flag : t -> flag -> int -> unit

val print_flags : t -> unit

