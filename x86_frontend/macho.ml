(* Copyright (c) 2013-2015, IMDEA Software Institute.          *)
(* See ../LICENSE for authorship and licensing information     *)

(* Data structures and parsing of MACH-O files *)

(* Module parameters *******************************************************)
let check = ref true

(* Types used to store all relevent informations ****************************)
type mach_header =
      { ncmds : int; (* number of load commands *)
        sizeofcmds : int; (* size of the sum of all load commmands *)
      }
type section_load_command =
  { sectname : string;
    sectsegname : string;
    sectaddr : int64;
    sectsize : int;
    sectoffset : int;
  }

type segment_load_command =
  { segname : string;
    vmaddr  : int64; 
    vmsize  : int;
    fileoff : int;
    filesize : int;
    nsects : int; (* number of sections *)
    segsections : section_load_command list;
  }

type unix_thread_command =
  { ut_eax : int64; (*initial values of registers *)
    ut_ebx : int64;
    ut_ecx : int64;
    ut_edx : int64;
    ut_edi : int64;
    ut_esi : int64;
    ut_ebp : int64;
    ut_esp : int64;
    ut_ss  : int64;
    ut_eflags : int64;
    ut_eip : int64; (* program counter *)
    ut_cs : int64;
    ut_ds : int64;
    ut_es : int64;
    ut_fs : int64;
    ut_gs : int64;
  }

type load_command = 
  Segment of segment_load_command
| UnixThread of unix_thread_command
| IgnoredCommand

type objectfile =
  { mach_header : mach_header;
    load_commands : load_command list;
(* plus link edit segment that contains symbols etc *)
  }

type t = objectfile

(* A few printing functions, to help debugging *******************************)
open Format
open X86Print


(* Now the parsing ***********************************************************)

exception NonMachOFile

open AsmUtil

let parse_header bits =
  let magic, bits = read_uint32 bits 32 in
  if magic = Int64.of_string "0xcefaedfe" then 
    failwith "Mac file, but big endian not supported"
  else if magic = Int64.of_string "0xfeedface" then (
    let cputype, bits = read_int32 bits 32 in
    let cpusubtype, bits = read_int32 bits 32 in
    let filetype, bits = read_uint32 bits 32 in
    if !check then (match (off_to_int filetype) with 
          1 -> Printf.printf "Object Mac file\n"
        | 2 -> Printf.printf "Executable Mac file\n"
        | 3 -> Printf.printf "Shared library file\n"
        | 4 -> Printf.printf "Core Mac file\n"
        | 5 -> Printf.printf "Preloaded executable file\n"
        | _ -> Printf.printf "Unknown type of Mac file\n"
                    );
    let ncmds, bits = read_uint32 bits 32 in
    let sizeofcmds, bits = read_uint32 bits 32 in
    let flags, bits = read_uint32 bits 32 in
      { ncmds = off_to_int ncmds;
        sizeofcmds = off_to_int sizeofcmds;
      }, bits
  ) else raise NonMachOFile

(*reads a string coded on 16 bytes *)
let read_string16 bits =
  let res, _ = read_string bits in res, skip bits 128

let parse_sec bits =
  let sectname, bits = read_string16 bits in
  let segname, bits = read_string16 bits in
  let addr, bits = read_uint32 bits 32 in
  let size, bits = read_uint32 bits 32 in
  let offset, bits = read_uint32 bits 32 in
  let align, bits = read_uint32 bits 32 in
  let reloc, bits = read_uint32 bits 32 in
  let nreloc, bits = read_uint32 bits 32 in
  let flags, bits = read_uint32 bits 32 in
  let reserved1, bits = read_uint32 bits 32 in
  let reserved2, bits = read_uint32 bits 32 in
  { sectname = sectname;
    sectsegname = segname;
    sectaddr = addr;
    sectsize = off_to_int size;
    sectoffset = off_to_int offset;
  }, bits

let rec parse_sections n acc bits = if n>0 then 
    let sec, bits = parse_sec bits in
    parse_sections (n-1) (sec::acc) bits  
  else List.rev acc, bits

let skip_load_command size bits = skip bits ((size-8)*8)

let parse_load_command bits =
  let cmd, bits = read_uint32 bits 32 in
  let cmdsize, bits = read_uint32 bits 32 in
  match (off_to_int cmd) with
    1 -> (* file segment to be mapped *)
      let name, bits = read_string16 bits in
      let vmaddr, bits = read_uint32 bits 32 in
      let vmsize, bits = read_uint32 bits 32 in
      let fileoff, bits = read_uint32 bits 32 in
      let filesize, bits = read_uint32 bits 32 in
      let maxprot, bits = read_int32 bits 32 in
      let initprot, bits = read_int32 bits 32 in
      let nsects, bits = read_uint32 bits 32 in
      let flags, bits = read_uint32 bits 32 in
      let sections, bits = parse_sections (off_to_int nsects) [] bits in
      Segment { segname = name;
                vmaddr  = vmaddr;
                vmsize  = off_to_int vmsize;
                fileoff = off_to_int fileoff;
                filesize = off_to_int filesize;
                nsects = off_to_int nsects;
                segsections = sections;
              }, bits
  | 5 -> (* Unix thread *)
         (* Data structure is machine dependent and declared in 
            /usr/include/mach-o/loaded.h *)
          let cmdunix = off_to_int cmdsize in
          if cmdunix>80 && !check then Printf.printf "Command size of LC_UNIXTHREAD is bigger than expected\n";
          if cmdunix<80 then failwith "Command size of LC_UNIXTHREAD too small\n";
          let flavor, bit2 = read_uint32 bits 32 in
          let count, bit2 = read_uint32 bit2 32 in (* soze of the following datat structure, in number of int32 *)
          let eax, bit2 = read_uint32 bit2 32 in
          let ebx, bit2 = read_uint32 bit2 32 in
          let ecx, bit2 = read_uint32 bit2 32 in
          let edx, bit2 = read_uint32 bit2 32 in
          let edi, bit2 = read_uint32 bit2 32 in
          let esi, bit2 = read_uint32 bit2 32 in
          let ebp, bit2 = read_uint32 bit2 32 in
          let esp, bit2 = read_uint32 bit2 32 in
          let ss, bit2 = read_uint32 bit2 32 in
          let eflags, bit2 = read_uint32 bit2 32 in
          let eip, bit2 = read_uint32 bit2 32 in (*the program counter*)
          let cs, bit2 = read_uint32 bit2 32 in
          let ds, bit2 = read_uint32 bit2 32 in
          let es, bit2 = read_uint32 bit2 32 in
          let fs, bit2 = read_uint32 bit2 32 in
          let gs, bit2 = read_uint32 bit2 32 in
          UnixThread { 
            ut_eax = eax;
            ut_ebx = ebx;
            ut_ecx = ecx;
            ut_edx = edx;
            ut_edi = edi;
            ut_esi = esi;
            ut_ebp = ebp;
            ut_esp = esp;
            ut_ss = ss;
            ut_eflags = eflags;
            ut_eip = eip;
            ut_cs = cs;
            ut_ds = ds;
            ut_es = es;
            ut_fs = fs;
            ut_gs = gs;
          }, skip_load_command (off_to_int cmdsize) bits
  | _ -> IgnoredCommand, skip_load_command (off_to_int cmdsize) bits

let rec parse_load_commands n acc bits =
  if n>0 then
    let lc, bits = parse_load_command bits in
    parse_load_commands (n-1) (lc::acc) bits
  else List.rev acc

let parse bits =
  let bits = goto bits 0 in
  let header, bits = parse_header bits in
  let load_commands = parse_load_commands header.ncmds [] bits in
  { mach_header = header;
    load_commands = load_commands;
  }

let read file = 
  Printf.printf "Reading file %s as a Mac executable\n" file;
  let bits = read_from_file file in parse bits

let rec find_unix_thread = function
  [] -> failwith "No unix thread\n"
| (UnixThread x)::_ -> x
| _::l -> find_unix_thread l

let virtual_start e = 
  let unix_thread = find_unix_thread e.load_commands in
  unix_thread.ut_eip

open ExecInterfaces

let lc_to_section_list = function
  Segment l -> List.map (fun s ->
        { start_addr = s.sectaddr;
          end_addr = Int64.add s.sectaddr (Int64.of_int s.sectsize);
          offset = s.sectoffset;
          max_valid_offset = s.sectoffset + s.sectsize;
        } ) l.segsections
| _ -> []

let sections e = List.flatten (List.map lc_to_section_list e.load_commands)
