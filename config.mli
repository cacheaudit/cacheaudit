(* Copyright (c) 2013-2015, IMDEA Software Institute.         *)
(* See ./LICENSE for authorship and licensing information     *)

(** Module for parsing the configuration file *)

open NumAD.DS

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
  int option * MemAD.mem_param * cache_params


type access_type = Instruction | Data
type stub_t = {
  first_addr : int64; (** first address of code to be stubbed *)
  next_addr : int64; (** the next address (after code being stubbed) *)
  accesses : (access_type * rw_t * int64) list; (**accesses to be emulated *)
}

(** Parse the stub file, which contains parameters for stubbing *)
val parse_stubfile : string -> stub_t list