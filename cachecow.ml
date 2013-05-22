open AsmUtil
open X86Headers
open Config
open Signatures

let bin_name = ref ""
let start_addr = ref(-1 )
let end_addr = ref 0
let cache_s = ref 0
let line_s = ref 0
let assoc = ref 0
let cache_strategy = ref Signatures.LRU

let build_cfg = ref false
let do_inter = ref false
let print_ass = ref false
let analyze = ref false
let prof = ref false
let interval_cache = ref false

type cache_age_analysis = IntAges | SetAges | OctAges | RelAges

let cache_analysis = ref SetAges

let cache_size = ref 0
let line_size = ref 0
let associativity = ref 0

type attacker_model = Final 
  | Instructions of int (* may interrupt each x instruction *)
  | OneInstrInterrupt
  | OneTimedInterrupt

let attacker = ref Final

let verbose_array = Array.init 5 (fun _ -> false)

let more_to_parse b = more b && (!end_addr=0 || get_byte b <= !end_addr)

(*let debug bits =
  let nubits=goto bits !start_addr in
  Format.printf "Address %Lx\n" (fst (AsmUtil.read_uint32 nubits 32))*)

let read_assembly bits =
  let rec read_instr_list b =
    if more_to_parse b then
      let addr = get_byte b in
      let i,nb = X86Parse.read_instr b in
      match i with
	  X86Types.Ret -> [(addr,i)]
      | _ -> (addr, i)::read_instr_list nb
    else []
  in read_instr_list (goto bits !start_addr)


let print_assembly bs = 
    List.iter (function (n,b) -> Format.printf "@<6>%n\t%x\t%a@\n" n n X86Print.pp_instr b) bs
    
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] [-f] BINARY"
(* function which handles binary names (anonymous arguments) *)
let anon_fun = (fun x ->  if !bin_name = "" then bin_name := x
               else raise (Arg.Bad ("The binary name is specified a second time: " ^ x)))

let speclist = [
    ("--start", Arg.String (fun s -> start_addr := int_of_string s), "set the address (in bytes) where we start parsing");
    ("--end", Arg.String (fun s -> end_addr := int_of_string s), "set the oddress (in bytes) where we stop parsing");
    ("--cfg", Arg.Unit (fun () -> build_cfg := true; print_ass := false), "build a control flow graph before printing");
    ("--silent", Arg.Unit (fun () -> print_ass := false),            "do not print the assembly code");
    ("--inter", Arg.Unit (fun () -> do_inter := true; print_ass := false),               "run the interpreter");
    ("--interverb", Arg.String (fun s -> 
            if String.contains s 'i' then verbose_array.(0) <- true;
            if String.contains s 'r' then verbose_array.(1) <- true;
            if String.contains s 'f' then verbose_array.(2) <- true;
            if String.contains s 's' then verbose_array.(3) <- true;
            if String.contains s 'c' then verbose_array.(4) <- true),
            "enable verbose output: i - print instructions; r - print registers; f - print flags; s - print stack accesses; c - print the cache");
    ("--analyze", Arg.Set analyze, "run analysis");
    ("--unroll", Arg.Int (fun u -> Iterator.unroll_count:=u), "number of loop unrollings");
    ("--noOuterUnroll", Arg.Unit (fun () -> Iterator.unroll_outer_loop:=false), "overwrites the --unroll option, so that outer loops are not unrolled");
    ("-f", Arg.String anon_fun,                      "give the name of the binary file");
    ("--oct", Arg.Unit (fun () -> cache_analysis := OctAges), "use the octagon abstract domain for the cache.") ;
    ("--interval-cache", Arg.Unit (fun () -> cache_analysis := IntAges), "use the interval abstract domain for the cache.") ;
    ("--rset", Arg.Unit (fun () -> cache_analysis := RelAges), "use the relational set abstract domain for the cache.") ;
    ("--prof", Arg.Unit (fun () -> prof := true), "collect and output additional profiling information for the cache.");
    ("--fifo", Arg.Unit (fun () -> cache_strategy := Signatures.FIFO), "sets the cache replacement strategy to FIFO instead of the default LRU.");
    ("--plru", Arg.Unit (fun () -> cache_strategy := Signatures.PLRU), "sets the cache replacement strategy to PLRU instead of the default LRU.");
     ("--cache-size", (Arg.Int (fun n -> cache_size := n)),
     "override the size of the cache (in bytes) from the configuration file");
    ("--line-size", (Arg.Int (fun n -> line_size := n)),
     "override the size of the cache lines (in bytes) from the configuration file");
    ("--assoc", (Arg.Int (fun n -> associativity := n)),
     "override the associativity (in bytes) from the configuration file");
    ("--instrAttacker", Arg.Int (fun d -> analyze:=true; attacker := Instructions d), "attacker may interrupt each d instruction (or more than d).");
   ("--oneInstrInterrupt", Arg.Unit (fun () -> analyze := true; attacker:=OneInstrInterrupt),"attacker that can interrupt only once per round, based on the number of instructions");
   ("--oneTimedInterrupt", Arg.Unit (fun () -> analyze:=true; attacker:=OneTimedInterrupt),"attacker that can interrupt only once per round, based on time")
  ] 

let _ =
  Arg.parse speclist anon_fun usage;
    if !bin_name="" then (Format.printf "Error: You need to specify a filename\n";
                          exit 1
                    );
  let start_values =
    begin
      try
          let sa,sv,cp = Config.config (!bin_name^".conf") in
          Printf.printf "Configuration file %s.conf parsed\n" !bin_name;
          (match sa with
            None -> Printf.printf "Start address not specified\n"
          | Some x -> Printf.printf "Start address is 0x%x\n" x; start_addr := x);
          cache_s := if !cache_size <= 0 then cp.cache_s else !cache_size;
          line_s := if !line_size <= 0 then cp.line_s else !line_size;
          assoc := if !associativity <= 0 then cp.assoc else !associativity;
          sv
      with Sys_error _ ->
        Printf.printf "Configuration file %s.conf not found\nUsing default values\n" !bin_name;
        List.map (fun (a,b) -> a,b,b) [(X86Types.EAX, 1L); (X86Types.ECX, 0xbffff224L); (X86Types.EDX, 0xbffff1b4L); (X86Types.EBX, 0x2d3ff4L); 
        (X86Types.ESP, 0xbffff18cL); (X86Types.EBP, 0L); (X86Types.ESI, 0L); (X86Types.EDI, 0L)]
    end
  in
  let bits, mem =
    try (
      let mem = read_exec !bin_name in
      (* Setting default values *)
      if !start_addr =(-1) then start_addr:=starting_offset mem;
      if !cache_s = 0 then cache_s := 16384;
      if !line_s = 0 then line_s := 32;
      if !assoc = 0 then assoc := 4;
      Printf.printf "Offset of first instruction is 0x%x (%d bytes in the file)\n" 
                !start_addr !start_addr;
      (get_bits mem), Some mem
    ) with Macho.NonMachOFile -> (
      Printf.printf "Not an ELF, or Mach-O file, entry point not determined\n";
      if !start_addr= -1 then start_addr:=0;
      (read_from_file !bin_name), None ) in
  if !print_ass then print_assembly (read_assembly bits);
  (*  if !print_ass then debug bits;*)
  let cache_params = (!cache_s,!line_s,!assoc,!cache_strategy) in
  match mem with
    Some sections ->
      if !build_cfg then Cfg.printcfg (Cfg.makecfg !start_addr sections);
      if !do_inter then Interpreter.interpret !bin_name sections !start_addr (List.map (fun (a,b,c) -> (a,b)) start_values) verbose_array;
      if !analyze then (
(* LM: that seems wrong to me. Does it mean we write in the executable? 
        SimpleOctAD.OctAD.set_bin_name !bin_name; *)
        let m = 
          if !prof then match !cache_analysis with
            OctAges -> (module CacheAD.ProfOctCacheAD : CACHE_ABSTRACT_DOMAIN)
          | RelAges ->  (module CacheAD.ProfRelSetCacheAD  : CACHE_ABSTRACT_DOMAIN)
          | SetAges -> (module CacheAD.ProfSimpleCacheAD : CACHE_ABSTRACT_DOMAIN)
          | IntAges -> failwith "Profiling for interval-based cache analysis not implemented\n"
        else match !cache_analysis with
            OctAges -> (module CacheAD.OctCacheAD : CACHE_ABSTRACT_DOMAIN)
          | RelAges -> (module RelCacheAD.RelSetCacheAD : CACHE_ABSTRACT_DOMAIN)
          | SetAges -> (module CacheAD.SimpleCacheAD : CACHE_ABSTRACT_DOMAIN)
          | IntAges -> (module CacheAD.IntervalCacheAD : CACHE_ABSTRACT_DOMAIN)
        in let module BaseCache = (val m: CACHE_ABSTRACT_DOMAIN) in
        let m = match !attacker with
          Final -> m
        | Instructions d -> AsynchronousAttacker.min_frequency := d;
            (module AsynchronousAttacker.InstructionBasedAttacker(BaseCache) :CACHE_ABSTRACT_DOMAIN)
        | OneInstrInterrupt -> (module AsynchronousAttacker.OneInstructionInterrupt(BaseCache) : CACHE_ABSTRACT_DOMAIN)
        | OneTimedInterrupt -> (module AsynchronousAttacker.OneTimeInterrupt(BaseCache) : CACHE_ABSTRACT_DOMAIN)
        in let module Cache = (val m: CACHE_ABSTRACT_DOMAIN) in
        let module Mem = MemAD.MemAD(FlagAD.FlagsAD)(Cache) in
        let module Stack = StackAD.StackAD(Mem) in
        let module Iter = Iterator.Build(Stack) in
        let iterate = Iter.iterate in
        let start = Sys.time () in 
        iterate sections start_values (Cfg.makecfg !start_addr sections) cache_params;
        Printf.printf "Analysis took %d seconds.\n" (int_of_float (Sys.time () -. start))
        )
  | None -> ()
