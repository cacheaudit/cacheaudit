type basicblock = {
  start_addr : int;
  end_addr : int;
  next_block_addr : int;
  content : (int * X86Types.instr) list;
  jump_command : X86Types.instr option;
  mutable out_edges : basicblock list;
  mutable in_edges : basicblock list;
}

val pp_block_addr : Format.formatter -> int -> unit
val pp_block_header : Format.formatter -> basicblock -> unit
val pp_block : Format.formatter -> basicblock -> unit


val makecfg : int -> X86Headers.t -> basicblock list
val printblock : basicblock -> unit
val printcfg : basicblock list -> unit
