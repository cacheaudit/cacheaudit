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

type cache_params =
{
    data_cache_s: int; (* in bytes *)
    data_line_s: int;  (* same as "data block size"; in bytes *)
    data_assoc: int;
    inst_cache_s: int; (* in bytes *)
    inst_line_s: int;  (* same as "data block size"; in bytes *)
    inst_assoc: int;
    inst_base_addr: int64;
}


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
  let rec auxmatch lss (start,registers,cache) =
    match lss with
    | [] -> (start,registers,cache)        
    | l::ls -> 
    (let (st,regs,ca) = auxmatch ls (start,registers,cache)
    in match l with
      | ("START",i,_) ->  (Some (Int64.to_int i), regs, ca)
      | ("data_cache_s",i,_) -> (st,regs,{ca with data_cache_s = Int64.to_int i})
      | ("data_line_s",i,_) -> (st,regs,{ca with data_line_s = Int64.to_int i})
      | ("data_assoc",i,_) -> (st,regs,{ca with data_assoc = Int64.to_int i})
      | ("inst_cache_s",i,_) -> (st,regs,{ca with inst_cache_s = Int64.to_int i})
      | ("inst_line_s",i,_) -> (st,regs,{ca with inst_line_s = Int64.to_int i})
      | ("inst_assoc",i,_) -> (st,regs,{ca with inst_assoc = Int64.to_int i})
      | ("INST_BASE",i,_) -> (st,regs,{ca with inst_base_addr = i})
      | ("",0L,_)  -> (st,regs,ca)
      | (str, l, h) ->
                   try (
                     (st, (X86Util.string_to_reg32 str, l, h) :: regs,ca)
                   ) with Invalid_argument arg -> failwith (Printf.sprintf "Configuration not supported. %s is not a valid register" arg)
            )
  in let empty_cparams = {data_cache_s = 0; data_line_s = 0; data_assoc = 0; inst_cache_s = 0; inst_line_s = 0; inst_assoc = 0; inst_base_addr = (Int64.of_int 0)}
  in auxmatch scanned (None,[],empty_cparams)
