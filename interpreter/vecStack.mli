type t

val empty : t

val push : t -> int64 -> unit
val pop : t -> int64

val is_empty : t -> bool
val empty_stack : t -> unit
