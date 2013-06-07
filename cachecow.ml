open AsmUtil
open X86Headers
open Config
open Signatures

let bin_name = ref ""
let start_addr = ref(-1 )
let end_addr = ref 0
let data_cache_s = ref 0
let data_line_s = ref 0
let data_assoc = ref 0
let inst_cache_s = ref 0
let inst_line_s = ref 0
let inst_assoc = ref 0
let data_cache_strategy = ref Signatures.LRU
let inst_cache_strategy_opt = ref None

let instruction_base_addr = ref (Int64.of_int 0)

let build_cfg = ref false
let print_ass = ref false
let analyze = ref false
let prof = ref false
let interval_cache = ref false
let do_traces = ref true

type cache_age_analysis = IntAges | SetAges | OctAges | RelAges

let data_cache_analysis = ref SetAges
let inst_cache_analysis_opt = ref None

let cache_size = ref 0
let line_size = ref 0
let associativity = ref 0

type attacker_model = Final 
  | Instructions of int (* may interrupt each x instruction *)
  | OneInstrInterrupt
  | OneTimedInterrupt

let attacker = ref Final

type architecture_model = Joint | Split | NoInstructionCache

let architecture = ref Split

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
    ("--analyze", Arg.Set analyze, "run analysis");
    ("--unroll", Arg.Int (fun u -> Iterator.unroll_count:=u), "number of loop unrollings");
    ("--noOuterUnroll", Arg.Unit (fun () -> Iterator.unroll_outer_loop:=false), "overwrites the --unroll option, so that outer loops are not unrolled");
    ("-f", Arg.String anon_fun,                      "give the name of the binary file");
    ("--oct", Arg.Unit (fun () -> data_cache_analysis := OctAges), "use the octagon abstract domain for the cache.") ;
    ("--interval-cache", Arg.Unit (fun () -> data_cache_analysis := IntAges), "use the interval abstract domain for the cache.") ;
    ("--rset", Arg.Unit (fun () -> data_cache_analysis := RelAges), "use the relational set abstract domain for the cache.") ;
    ("--prof", Arg.Unit (fun () -> prof := true), "collect and output additional profiling information for the cache.");
    ("--fifo", Arg.Unit (fun () -> data_cache_strategy := Signatures.FIFO), "sets the cache replacement strategy to FIFO instead of the default LRU.");
    ("--plru", Arg.Unit (fun () -> data_cache_strategy := Signatures.PLRU), "sets the cache replacement strategy to PLRU instead of the default LRU.");
    ("--inst-oct", Arg.Unit (fun () -> inst_cache_analysis_opt := Some OctAges), "use the octagon abstract domain for the cache.") ;
    ("--inst-interval-cache", Arg.Unit (fun () -> inst_cache_analysis_opt := Some IntAges), "use the interval abstract domain for the cache.") ;
    ("--inst-rset", Arg.Unit (fun () -> inst_cache_analysis_opt := Some RelAges), "use the relational set abstract domain for the cache.") ;
    ("--inst-fifo", Arg.Unit (fun () -> inst_cache_strategy_opt := Some Signatures.FIFO), "sets the cache replacement strategy to FIFO instead of the default LRU.");
    ("--inst-plru", Arg.Unit (fun () -> inst_cache_strategy_opt := Some Signatures.PLRU), "sets the cache replacement strategy to PLRU instead of the default LRU.");
     ("--cache-size", (Arg.Int (fun n -> data_cache_s := n)),
     "override the size of the cache (in bytes) from the configuration file");
    ("--line-size", (Arg.Int (fun n -> data_line_s := n)),
     "override the size of the cache lines (in bytes) from the configuration file");
    ("--assoc", (Arg.Int (fun n -> data_assoc := n)),
     "override the associativity (in bytes) from the configuration file");
    ("--inst-cache-size", (Arg.Int (fun n -> inst_cache_s := n)),
     "override the size of the instruction cache (in bytes) from the configuration file");
    ("--inst-line-size", (Arg.Int (fun n -> inst_line_s := n)),
     "override the size of the instruction cache lines (in bytes) from the configuration file");
    ("--inst-assoc", (Arg.Int (fun n -> inst_assoc := n)),
     "override the instruction cache associativity (in bytes) from the configuration file");
     ("--cache-size", (Arg.Int (fun n -> cache_size := n)),
     "override the size of the cache (in bytes) from the configuration file");
    ("--line-size", (Arg.Int (fun n -> line_size := n)),
     "override the size of the cache lines (in bytes) from the configuration file");
    ("--assoc", (Arg.Int (fun n -> associativity := n)),
     "override the associativity (in bytes) from the configuration file");
    ("--instrAttacker", Arg.Int (fun d -> analyze:=true; attacker := Instructions d), "attacker may interrupt each d instruction (or more than d).");
    ("--oneInstrInterrupt", Arg.Unit (fun () -> analyze := true; attacker:=OneInstrInterrupt),"attacker that can interrupt only once per round, based on the number of instructions");
    ("--oneTimedInterrupt", Arg.Unit (fun () -> analyze:=true; attacker:=OneTimedInterrupt),"attacker that can interrupt only once per round, based on time");
    ("--jointArchitecture", Arg.Unit (fun () -> architecture := Joint), "Use the same cache for instructios and data.");
    ("--noInstructionCache", Arg.Unit (fun () -> architecture := NoInstructionCache),"Don't take into account the instruction cache");
    ("--noTraces", Arg.Unit (fun () -> do_traces := false),"Disable tracking of traces (and time)");
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
          instruction_base_addr := cp.inst_base_addr;
          data_cache_s := if !data_cache_s <= 0 then cp.data_cache_s else !data_cache_s;
          data_line_s := if !data_line_s <= 0 then cp.data_line_s else !data_line_s;
          data_assoc := if !data_assoc <= 0 then cp.data_assoc else !data_assoc;
          inst_cache_s := if !inst_cache_s <= 0 then cp.inst_cache_s else !inst_cache_s;
          inst_line_s := if !inst_line_s <= 0 then cp.inst_line_s else !inst_line_s;
          inst_assoc := if !inst_assoc <= 0 then cp.inst_assoc else !inst_assoc;
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
      if (Int64.compare !instruction_base_addr Int64.zero) = 0 then instruction_base_addr := 139844135157760L;
      if !data_cache_s = 0 then data_cache_s := 16384;
      if !data_line_s = 0 then data_line_s := 32;
      if !data_assoc = 0 then data_assoc := 4;
      if !inst_cache_s = 0 then inst_cache_s := !data_cache_s;
      if !inst_line_s = 0 then inst_line_s := !data_line_s;
      if !inst_assoc = 0 then inst_assoc := !data_assoc;
	  Printf.printf "Cache size %d, line size %d, associativity %d\n" !data_cache_s !data_line_s !data_assoc;
      Printf.printf "Offset of first instruction is 0x%x (%d bytes in the file)\n" 
                !start_addr !start_addr;
      (get_bits mem), Some mem
    ) with Macho.NonMachOFile -> (
      Printf.printf "Not an ELF, or Mach-O file, entry point not determined\n";
      if !start_addr= -1 then start_addr:=0;
      (read_from_file !bin_name), None ) in
  if !print_ass then print_assembly (read_assembly bits);
  (*  if !print_ass then debug bits;*)
  let data_cache_params = (!data_cache_s,!data_line_s,!data_assoc,!data_cache_strategy) in
  let inst_cache_strategy = match !inst_cache_strategy_opt with
    | Some(v) -> ref v 
    | None -> data_cache_strategy in
  let inst_cache_params = (!inst_cache_s,!inst_line_s,!inst_assoc,!inst_cache_strategy) in
  let inst_cache_analysis = match !inst_cache_analysis_opt with
    | Some(v) -> ref v
    | None -> data_cache_analysis in

  match mem with
    Some sections ->
      if !build_cfg then Cfg.printcfg (Cfg.makecfg !start_addr sections);
      if !analyze then (
(* LM: that seems wrong to me. Does it mean we write in the executable? 
        SimpleOctAD.OctAD.set_bin_name !bin_name; *)
        let generate_cache prof cache_analysis attacker =
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
          match !attacker with
              Final -> m
            | Instructions d -> AsynchronousAttacker.min_frequency := d;
                (module AsynchronousAttacker.InstructionBasedAttacker(BaseCache) :CACHE_ABSTRACT_DOMAIN)
            | OneInstrInterrupt -> (module AsynchronousAttacker.OneInstructionInterrupt(BaseCache) : CACHE_ABSTRACT_DOMAIN)
            | OneTimedInterrupt -> (module AsynchronousAttacker.OneTimeInterrupt(BaseCache) : CACHE_ABSTRACT_DOMAIN)

        in let m = generate_cache prof data_cache_analysis attacker
        in let module Cache = (val m: CACHE_ABSTRACT_DOMAIN) in
        let trcs = if !do_traces then 
          (module TraceAD.TraceAD(Cache): TRACE_ABSTRACT_DOMAIN)
        else (module TraceAD.NoTraceAD(Cache)) in
        let module Traces = (val trcs) in
        let module Mem = MemAD.MemAD(FlagAD.FlagsAD)(Traces) in
        let module Stack = StackAD.StackAD(Mem) in
        let arch = match !architecture with
            Split -> let m = generate_cache prof inst_cache_analysis attacker in
                     let module InstCache = (val m: CACHE_ABSTRACT_DOMAIN) in
                      (module ArchitectureAD.SplitCacheArchitectureAD(Stack)(InstCache): ARCHITECTURE_ABSTRACT_DOMAIN)
          | Joint -> (module ArchitectureAD.JointCacheArchitectureAD(Stack): ARCHITECTURE_ABSTRACT_DOMAIN)
          | NoInstructionCache -> (module ArchitectureAD.NoInstructionCacheArchitectureAD(Stack): ARCHITECTURE_ABSTRACT_DOMAIN) in
        let module Architecture = (val arch: ARCHITECTURE_ABSTRACT_DOMAIN) in
        let module Iter = Iterator.Build(Architecture) in
        let iterate = Iter.iterate in
        let start = Sys.time () in 
        iterate sections start_values (Cfg.makecfg !start_addr sections) data_cache_params (Some(inst_cache_params)) !instruction_base_addr;
        Printf.printf "Analysis took %d seconds.\n" (int_of_float (Sys.time () -. start))
        )
  | None -> ()
