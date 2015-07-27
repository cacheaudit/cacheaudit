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

type cache_params = {
  cache_s : int;
  line_s : int;
  assoc : int;
  inst_cache_s : int;
  inst_line_s : int;
  inst_assoc : int;
  inst_base_addr : int64;
}

(** Parse the configuration file, which contains the cache parameters and start address *)
val parse_conffile :
  string ->
  int option * mem_param * cache_params


type access_type = Instruction | Data
type stub_t = {
  first_addr : int; (** first address of code to be stubbed *)
  next_addr : int; (** the next address (after code being stubbed) *)
  accesses : (access_type * rw_t * int64) list; (**accesses to be emulated *)
}

type stubs_t = stub_t list

val get_stub : int -> stub_t list -> stub_t option

(** Parse the stub file, which contains parameters for stubbing *)
val parse_stubfile : string -> stubs_t

val logged_addresses : int64 list ref