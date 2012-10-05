(* Builds controle flow graph *)

type basicblock =
    {
      start_addr : int;                         (* start address of basic block *)
      end_addr : int;
      next_block_addr : int;
      content : (int * X86Types.instr) list;       (* address,instruction-pairs *)
      jump_command : X86Types.instr option; (* final instruction, does not appear in content *)
      mutable out_edges : basicblock list;
      mutable in_edges: basicblock list
    }


val pp_block_addr : Format.formatter -> int -> unit
val pp_block_header : Format.formatter -> basicblock -> unit
val pp_block : Format.formatter -> basicblock -> unit

val addr_ending_block : int
val addr_starting_block : int
val addr_error_block : int

(* The first element of the output cfg is the starting block *)
val makecfg : int -> X86Headers.t -> basicblock list

val printcfg : basicblock list -> unit
