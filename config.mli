val left_pos : string -> int -> int option
val right_pos : string -> int -> int option
val trim : string -> string
val read_lines : string -> string list
type cache_params = {
  data_cache_s : int;
  data_line_s : int;
  data_assoc : int;
  inst_cache_s : int;
  inst_line_s : int;
  inst_assoc : int;
  inst_base_addr : int64;
}
val config :
  string ->
  int option * (((int64 * int64 * int64) list)*((X86Types.reg32 * int64 * int64) list)) * cache_params
