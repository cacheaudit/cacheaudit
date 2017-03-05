(* Copyright (c) 2013-2017, IMDEA Software Institute.         *)
(* See ./LICENSE for authorship and licensing information     *)

open X86Types
open NumAD.DS
open Utils

let logged_addresses = ref []

(** Appends an address to the list of addresses that are logged.  The
    ordering of values in the log file corresponds to the ordering of
    that list.  Whenever there is a call to print, all addresses are
    logged *)
let log_address addr =
  logged_addresses := !logged_addresses @ [addr]


(* help functions for trimming a string *)
let left_pos s len =
  let rec aux i =
    if i >= len then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (succ i)
    | _ -> Some i
  in
  aux 0
 
let right_pos s len =
  let rec aux i =
    if i < 0 then None
    else match s.[i] with
    | ' ' | '\n' | '\t' | '\r' -> aux (pred i)
    | _ -> Some i
  in
  aux (pred len)
 
(* trim a string - remove whitespace before and after string*)
let trim s =
  let len = String.length s in
  match left_pos s len, right_pos s len with
  | Some i, Some j -> String.sub s i (j - i + 1)
  | None, None -> ""
  | _ -> assert false


(* Returns a list of lines in file filename *)
let read_lines filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; []
with End_of_file ->
  close_in chan; !lines

type reg_init_values = (X86Types.reg32 * int64 * int64) list
type mem_init_values = (int64 * int64 * int64) list
type mem_param = mem_init_values * reg_init_values

(* type cache_params =                                              *)
(* {                                                                *)
(*     cache_s: int; (* in bytes *)                                 *)
(*     line_s: int;  (* same as "data block size"; in bytes *)      *)
(*     assoc: int;                                                  *)
(*     inst_cache_s: int; (* in bytes *)                            *)
(*     inst_line_s: int;  (* same as "data block size"; in bytes *) *)
(*     inst_assoc: int;                                             *)
(*     inst_base_addr: int64;                                       *)
(* }                                                                *)

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

type access_type = Instruction | Data
type stub_t = {
  first_addr : int;
  next_addr : int;
  accesses : (access_type * rw_t * int64 * int64 option) list;
}

type stubs_t = stub_t list

let check_interval lower upper =
  (* Allow the initial value -1, which signifies a symbolic value *)
  if lower = upper && lower = -1L then true
  else
    let check_range n = n >= 0L && n <= 0xFFFFFFFFL in
    check_range lower && check_range upper && (upper >= lower)

let remove_comments_whitespace lines = 
  let lines = List.map (fun x ->
        (* Remove comments *)
        let x = if String.contains x '#' then
                  String.sub x 0 (String.index x '#')
                else x
        in
        (* Remove whitespaces *)
        trim x) lines in
  (* Remove empty lines *)
  List.filter (fun x -> x <> "") lines

let parse_conffile filename =
  let lines = remove_comments_whitespace (read_lines filename) in
  (* Accept intervals as starting values. *)
  (* Scanned returns a list of strings with two numbers; *)
  (* the two numbers correspond to an interval *)
  let scanned = List.map (fun x ->
      if x = "" then ("",0L,0L)
        else 
          try Scanf.sscanf x "%s %Li" (fun s i -> (s,i,i))
          with Scanf.Scan_failure _ -> Scanf.sscanf x "%s [%Li, %Li]" (fun s l h -> (s,l,h))
        ) lines in
  (* let rec auxmatch lss (start,registers,cache) = *)
  let rec auxmatch lss configs =
    match lss with
    | [] -> configs
    | l::ls -> 
     let cfgs = match l with
      | ("START",i,_) -> {configs with start_addr = Some (Int64.to_int i)}
      | ("END",i,_) ->  {configs with end_addr = Some (Int64.to_int i)}
      | ("cache_s",i,_) -> {configs with cache_s = Some (Int64.to_int i)}
      | ("line_s",i,_) -> {configs with line_s = Some (Int64.to_int i)}
      | ("assoc",i,_) -> {configs with assoc = Some (Int64.to_int i)}
      | ("inst_cache_s",i,_) -> {configs with inst_cache_s = Some (Int64.to_int i)}
      | ("inst_line_s",i,_) -> {configs with inst_line_s = Some (Int64.to_int i)}
      | ("inst_assoc",i,_) -> {configs with inst_assoc = Some (Int64.to_int i)}
      | ("INST_BASE",i,_) -> {configs with inst_base_addr = Some i}
      | ("LOG",i,_) -> log_address i; configs
      | ("",0L,_)  -> configs
      | (str, l, h) ->
        if not (check_interval l h) then 
          failwith "Invalid range of initial values [%Lx, %Lx].\n 
          Values should fit into 32 bits, first value <= second value";
        let mem,regs = configs.mem_params in
        let mem_params = 
          try (mem, (X86Util.string_to_reg32 str, l, h) :: regs)
          with Invalid_argument _ -> try 
            ((Int64.of_string str, l, h) :: mem, regs) 
            with Failure arg -> failwith (Printf.sprintf "Configuration not 
            supported. %s is not a valid register or a memory location" arg) in
         { configs with mem_params = mem_params }
    in auxmatch ls cfgs in
  let empty_params = 
        {start_addr = None; end_addr = None; cache_s = None; line_s = None;
        assoc = None; inst_cache_s = None; inst_line_s = None; 
        inst_assoc = None; inst_base_addr = None; mem_params = ([],[]);}
      in auxmatch scanned empty_params

exception StubParseFailed of string

let parse_stubfile filename = 
  try
    let lines = remove_comments_whitespace (read_lines filename) in
    let stubs = List.fold_right (fun l state -> 
      try 
        Scanf.sscanf l "range %i %i" (fun start next -> 
          {first_addr = start; next_addr = next; accesses = []} :: state)
      with Scanf.Scan_failure _ -> 
        try
          Scanf.sscanf l "M %Li %Li" (fun addr value -> 
            match state with
            | s :: stbs -> {s with 
              accesses = (Data, Write, addr, Some value) :: s.accesses} :: stbs
            | _ -> raise (StubParseFailed "Wrong stub format. \n
            Memory is expeted to be set as\nM addr 123"))
        with Scanf.Scan_failure _ -> 
          try Scanf.sscanf l "M %s %Li" (fun reg value -> 
            match state with
            | s :: stbs -> {s with 
              accesses = (Data, Write, Utils.reg_to_var (X86Util.string_to_reg32 reg), Some value) :: s.accesses} :: stbs
            | _ -> raise (StubParseFailed "Wrong stub format. \n
            Registers are expeted to be set as\nM EAX 123\n (EAX is set to 123; value -1 would mean symbolic)"))
          with Scanf.Scan_failure _ -> 
            Scanf.sscanf l "%s %s %Li" (fun acctype rw addr ->
            let acctype = match acctype with 
            | "D" -> Data | "I" -> Instruction
            | _ -> raise (StubParseFailed "Access type should be D or I") in
            let rw = match rw with
            | "R" -> Read | "W" -> Write
            | _ -> raise (StubParseFailed "Access should be either R (read) or W (write)") in
            match state with
            | s :: stbs -> {s with 
              accesses = (acctype, rw, addr, None) :: s.accesses} :: stbs
            | _ -> raise (StubParseFailed "Wrong stub format. Expected format:\
            \nrange 0xe5d 0xe6b\nI R 0xe3c\nD W 0xe3d\nrange ..."))
      ) lines [] in
    (* Reverse the order of sequence of instructions *)
    let stubs = List.map (fun stub -> 
      {stub with accesses = List.rev stub.accesses}) stubs in
    (* Sort stubs by address *)
    List.sort (fun s1 s2 -> Pervasives.compare s1.first_addr s2.first_addr) stubs
  with StubParseFailed msg -> failwith ("Failed parsing the stub file: " ^ msg)

let get_stub addr stubs = 
  List.fold_left (fun stub_found stub ->
    if stub_found = None && stub.first_addr = addr then
        Some stub
      else 
        stub_found) None stubs
