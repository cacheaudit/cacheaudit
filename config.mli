(* Copyright (c) 2013-2015, IMDEA Software Institute.         *)
(* See ./LICENSE for authorship and licensing information     *)

(** Module for parsing the configuration file *)

open NumAD.DS

(** List of initial values for registers. Register * lower bound * upper bound *)
type reg_init_values = (X86Types.reg32 * int64 * int64) list

(** List of initial values for memory addresses. Adress * lower bound * upper bound *)
type mem_init_values = (int64 * int64 * int64) list

(** Parameters for the Memory Abstract Domain initialization *)
type mem_param = mem_init_values * reg_init_values


val left_pos : string -> int -> int option
val right_pos : string -> int -> int option
val trim : string -> string
val read_lines : string -> string list

type config_options =
{
    start_addr: int option;
    end_addr: int option;
    cache_s: int option; (* in bytes *)
    line_s: int option;  (* same as "data block size"; in bytes *)
    assoc: int option;
    inst_cache_s: int option; (* in bytes *)
    inst_line_s: int option;  (* same as "data block size"; in bytes *)
    inst_assoc: int option;
    inst_base_addr: int64 option;
    mem_params: mem_param;
}

(** Parse the configuration file, which contains the cache parameters and start address *)
val parse_conffile :
  string -> config_options


type access_type = Instruction | Data
type stub_t = {
  first_addr : int; (** first address of code to be stubbed *)
  next_addr : int; (** the next address (after code being stubbed) *)
  accesses : (access_type * rw_t * int64 * int64 option) list; (**accesses to be emulated *)
}

type stubs_t = stub_t list

val get_stub : int -> stub_t list -> stub_t option

(** Parse the stub file, which contains parameters for stubbing *)
val parse_stubfile : string -> stubs_t

val logged_addresses : int64 list ref

