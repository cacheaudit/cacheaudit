(* Copyright (c) 2013-2015, IMDEA Software Institute.         *)
(* See ./LICENSE for authorship and licensing information     *)

open X86Types

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

let setInitialValue addr lower upper mem =
  let parse_interval_bound n =
    if Int64.compare n 0L = -1 then failwith "Negative numbers are not allowed"
    else if Int64.compare n 0xFFFFFFFFL = 1 then failwith "Numbers have to be in the 32bit range"
    else n in
  let lower = parse_interval_bound lower in
  let upper = parse_interval_bound upper in
  if Int64.compare lower upper = 1 then failwith "lower bound should be lower or equal than upper bound"
  else (addr,lower,upper) :: mem



let config filename =
  let scanned = 
    List.map (fun x ->
        (* Remove whitespaces *)
        let x = trim x in
        (* Remove comments *)
        let x = if String.contains x '#' then
                  String.sub x 0 (String.index x '#')
                else x
        in
        if x = "" then ("",0L,0L)
        else 
          (* Quick fix in order to accept intervals as starting values *)
          try Scanf.sscanf x "%s %Li" (fun s i -> (s,i,i))
          with Scanf.Scan_failure _ -> Scanf.sscanf x "%s [%Li, %Li]" (fun s l h -> (s,l,h))
        ) (read_lines filename) in
  (* scanned returns a list of strings with two numbers; the two numbers correspond to an interval *)
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
      | ("LOG",i,_) -> MemAD.log_address i; configs
      | ("",0L,_)  -> configs
      | (str, l, h) ->
        let mem,regs = configs.mem_params in
                  try (
                    {configs with mem_params = (mem, (X86Util.string_to_reg32 str, l, h) :: regs)}
                  ) with Invalid_argument arg -> try (
                    {configs with mem_params = (setInitialValue (Int64.of_string str) l h mem, regs)}
                  ) with Failure arg -> failwith (Printf.sprintf "Configuration not supported. %s is not a valid register or a memory location" arg)
     in auxmatch ls cfgs in
  let empty_params = 
        {start_addr = None; end_addr = None; cache_s = None; line_s = None;
        assoc = None; inst_cache_s = None; inst_line_s = None; 
        inst_assoc = None; inst_base_addr = None; mem_params = ([],[]);}
      in auxmatch scanned empty_params
