(* Copyright (c) 2013-2015, IMDEA Software Institute.         *)
(* See ./LICENSE for authorship and licensing information     *)

(** Module for parsing the configuration file *)

val left_pos : string -> int -> int option
val right_pos : string -> int -> int option
val trim : string -> string
val read_lines : string -> string list
(* type cache_params = {     *)
(*   cache_s : int;          *)
(*   line_s : int;           *)
(*   assoc : int;            *)
(*   inst_cache_s : int;     *)
(*   inst_line_s : int;      *)
(*   inst_assoc : int;       *)
(*   inst_base_addr : int64; *)
(* }                         *)

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
    mem_params: MemAD.mem_param;
}

val config :
  string -> config_options
