module IntSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = int
  end )

module EdgeSet = Set.Make(
  struct
    let compare=Pervasives.compare
    type t=int*int
  end)

open X86Types
open AsmUtil

let verbose = ref false

(*Control Flow Graph*)

type basicblock =
    {
      start_addr : int;                         (* start address of basic block *)  
      end_addr : int;
      next_block_addr : int;
      content : (int * instr) list;       (* address,instruction-pairs *)
      jump_command : instr option; (* final instruction, does not appear in content *)
      mutable out_edges : basicblock list;
      mutable in_edges: basicblock list
    }      
type t = basicblock list


let addr_ending_block   = -1
let addr_starting_block = -2
let addr_error_block    = -3

let new_block sa en nb cont = 
  let rec extract_last = function
      [] -> failwith "Empty list argument to extract_last\n"
    | [l] -> [], l
    | x::ll -> let beg,l = extract_last ll in x::beg,l
  in
  let c,j = match cont with
      [] -> [], None
    | _ -> let beg,(_,l) = extract_last cont in
      ( match l with
        Jcc _ | Jmp _ | Ret | Halt | Call _ -> beg, Some l
      | _ -> cont, None
      )
  in { start_addr = sa;
       end_addr   = en;
       next_block_addr =  nb;
       content    = c;
       jump_command = j;
       out_edges  = [];
       in_edges   = [];
     }

(*Print block *)
let pp_block_addr fmt n = 
  if n = addr_starting_block then Format.fprintf fmt "@ START"
  else if n = addr_error_block then Format.fprintf fmt "@ ERROR"
  else if n = addr_ending_block then Format.fprintf fmt "@ FINAL"
  else Format.fprintf fmt "@<6>0x%x" n
 
let pp_block_header fmt b =
  Format.fprintf fmt "Start: %a @ End: %a @ Next: %a@."
    pp_block_addr b.start_addr pp_block_addr b.end_addr
    pp_block_addr b.next_block_addr
 
let pp_block fmt b =
  Format.fprintf fmt "@[%a@[" pp_block_header b;
  List.iter (function (a,c) -> Format.fprintf fmt "%a %a@." 
          pp_block_addr a X86Print.pp_instr c) b.content;
  ( match b.jump_command with None -> ()
  | Some ins -> Format.fprintf fmt "%a %a@." pp_block_addr b.end_addr X86Print.pp_instr ins
  );
  Format.printf "Edges to: "; List.iter (function x->Format.fprintf fmt "%a@ " pp_block_addr x.start_addr) b.out_edges;
  Format.printf"@.@]\n@]"

(* Returns last element of list, fails if list is empty *)
let rec last = function
  | [x] -> x
  | (x::xs) -> last xs
  | _ -> failwith "Empty lists have no last elements"



(* Takes a section and an X86Types.address types and returns the value stored there *)
(* May raise X86Headers.InvalidVirtualAddress *)
let strip_lookup sect addr =
  match addr.addrBase with
    | Some _ -> failwith "Relative addressing failure"
    | None -> match addr.addrIndex with
	| Some _ -> failwith "Relative addressing failure"
	| None -> match addr.segBase with
	    | Some _ ->  failwith "Relative addressing failure"
	    | None -> 
	      let y = X86Headers.lookup sect addr.addrDisp in
	      let x=X86Headers.virtual_to_offset sect y
	      in x 

(* The function that checks if we should recurse *)
let rec_call edge edges f =
  if EdgeSet.mem edge edges then edges
  else f (EdgeSet.add edge edges)

(* 
   getaddresses: section,  bits |-> EdgeSet
   Collects edges of the CFG in form (source address, target address)
*)

let getedges sections bs =
  if !verbose then Format.printf "getedges \n";
  let rec getaux context b edges = 
    if more b then
      let i,nb = X86Parse.read_instr b in
      let src = get_byte b in
      let nsrc = get_byte nb in
      if !verbose then begin
        Format.printf "Context %a \t" pp_block_addr context; 
        Format.printf "@<6>%x\t%a@\n" (get_byte b) X86Print.pp_instr i
      end;
      match i with 
	| Jcc (_,x) -> 
	  let tgt = Int64.to_int x in
	  let edge1 = (src, tgt) in 
	  let edge2 = (src, nsrc) in
	  let nedges = rec_call edge1 edges (getaux context (goto b tgt)) in
    rec_call edge2 nedges (getaux context nb)
	(* Three cases JmpInd *)
	| Jmp (Imm x) ->
	  let tgt = Int64.to_int x in 
	  let edge = (src,tgt) in
    rec_call edge edges (getaux context (goto b tgt))
	| Jmp (Address x) ->
	  (try
      let tgt = strip_lookup sections x in 
		  let edge = (src,tgt) in
      rec_call edge edges (getaux context (goto b tgt))
	   with X86Headers.InvalidVirtualAddress -> EdgeSet.add (src,addr_error_block) edges)
	| Jmp (Reg _) -> failwith "Relative addressing failure"
	| Call (Imm x) ->  
	  let tgt = Int64.to_int x in 
	  let edge = (src,tgt) in
    let nedges = rec_call edge edges (getaux nsrc (goto b tgt)) in
(* now we add the edges to the rest of the current context *)
    getaux context nb nedges
	| Call (Address x) ->  
	  let nedges = (try
      let tgt = strip_lookup sections x in 
	    let edge = (src,tgt) in
      rec_call edge edges (getaux nsrc (goto b tgt))
	   with X86Headers.InvalidVirtualAddress -> EdgeSet.add (src,addr_error_block) edges) in
    getaux context nb nedges
	| Call (Reg _) -> failwith "Relative addressing failure"
	| Ret -> EdgeSet.add (src,context) edges  (*return to calling context *)
	| _ -> getaux context nb edges
    else edges
  in getaux addr_ending_block bs EdgeSet.empty
  


(* 
   val collectbasicblocks : bits -> ((int * instr) list , int list) list
   first component of return value is basic block,
   second component is list of targets of outgoing edges
   basic block starts <=> address is jump target bs
   basic block ends  <=> (1) Jmp x etc  *or* (2) next address in getaddresses bs 
*)   

let collectbasicblocks edges bs =
  if !verbose then Format.printf "collectbasicblocks \n";
  (* Collect all nonnegative jmp targets *)
  let targets = EdgeSet.fold (fun (_,tgt) set -> if tgt>=0 then IntSet.add tgt set else set) edges IntSet.empty in
  let rec read_block ad = 
    let c,nb = X86Parse.read_instr (goto bs ad) in
    let nad = get_byte nb in
    match c with                          (* block termination condition 1 *)
      | Jmp _ | Jcc _ | Call _ | Ret | Halt -> (([(ad,c)],nad), None)
      | _ -> 
	     if IntSet.mem nad targets                     (* block termination condition 2 *)
	     then (([(ad,c)],nad), Some(ad,nad))
	     else let (cs,nad),es = read_block nad in ((((ad,c)::cs),nad),es)
  in 
  IntSet.fold 
    (fun ad (cs,es) -> 
        let (c,e) = read_block ad in 
        (c::cs , match e with None -> es | Some e -> EdgeSet.add e es)
    ) targets ([],EdgeSet.empty)
			 
(* Builds cfg from section info and start address *)

let makecfg start sections=
  let bs = goto (X86Headers.get_bits sections) start in
  let makeblock (commands, nb) = new_block (fst (List.hd commands)) 
      (fst (last commands)) nb commands in
  (* pre_edges are those that correspond to blocks that end with jmps etc
     post_edges are those that correspond to blocks that end by being jmped to.
     post_edges are determined when computing basic blocks *)
  let pre_edges = EdgeSet.add (addr_starting_block, start) 
                              (getedges sections bs) in 
  if !verbose then Format.printf "got Edges\n";
  let pre_blocks, post_edges = collectbasicblocks pre_edges bs in
  let edges = EdgeSet.union pre_edges post_edges in
  let start_block = new_block addr_starting_block addr_starting_block 0 [] in
  let basicblocks = 
    [start_block; new_block addr_ending_block 0 0 []; 
     new_block addr_error_block 0 0 []] 
      @ (List.map makeblock pre_blocks) in
  EdgeSet.iter 
    (fun (src,tgt) -> try
			let src_block =  List.find (fun x -> x.end_addr = src) basicblocks in
			let tgt_block =  List.find (fun x -> x.start_addr = tgt) basicblocks in
			src_block.out_edges <- tgt_block::src_block.out_edges;
			tgt_block.in_edges <- src_block :: tgt_block.in_edges
      with Not_found -> (Printf.printf "Edge (0x%x, 0x%x) not found\n" src tgt; raise Not_found)
    ) edges;
  if !verbose then Format.printf "Got graph\n"; 
  basicblocks




(* Prints control flow graph, blocks separated by newline *)
let printblock = pp_block Format.std_formatter

let printcfg g = Format.printf "CFG is \n";List.iter printblock g

