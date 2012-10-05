type t

val init : unit -> t
val set_ebp : t -> int64 -> unit

(* get 32 (actually 64) bits of memory 
  (actually 64 half of which should stay unused) *)
val get : t -> int64 -> int64
val set : t -> int64 -> int64 -> unit

(* get 8 bits of memory *)
val get_byte : t -> int64 -> int64
val set_byte : t -> int64 -> int64 -> unit

val push : t -> int64 -> unit
val pop : t -> int64
val update_esp: t -> int64 -> unit

val is_empty : t -> bool
(* val empty_stack : t -> unit *)

(* Check whether memory location is in stack *)
val in_stack : t -> int64 -> bool

val set_verbose : t -> bool -> unit
