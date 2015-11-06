(* Copyright (c) 2013-2015, IMDEA Software Institute.          *)
(* See ../LICENSE for authorship and licensing information     *)

open X86Types
open AbstrInstr
open AD.DS

module type S = 
  sig
    include AD.S

    val init : X86Headers.t -> Config.mem_param -> CacheAD.cache_param_t -> t
    val get_vals : t -> op32 -> (int, t) finite_set
    val test : t -> condition -> t add_bottom * t add_bottom
    val call : t -> op32 -> int -> (int, t) finite_set
    val return : t -> (int, t) finite_set
    val interpret_instruction : t -> X86Types.instr -> t
    val touch : t -> int64 -> NumAD.DS.rw_t -> t
    val set_value: t -> int64 -> int64 -> t
    val elapse : t -> int -> t
  end


(* Simple stack abstract domain. Translates pushs and pops to register operations *)

module Make (M: MemAD.S) = struct
    (* Stack.top and Memory abstract domain *)
  type t = M.t

  let init mem mem_params cache_params = M.init (fun addr -> 
    if addr=Int64.zero then failwith "impossible memory address 0\n"
    else try Some(X86Headers.lookup mem addr) with 
        (*We then assume it is not initialzed*)
      X86Headers.InvalidVirtualAddress -> None) mem_params cache_params (*TDO check that it falls in the stack *)
  let join = M.join 
  let widen = M.widen 
  let subseteq = M.subseteq 
  let get_vals  = M.get_vals
  let test = M.test
  let top_stack =  Address {  addrDisp = 0L;
                              addrBase = Some ESP;
                              addrIndex = None;
                              segBase = None;}
  
  let interpret_instruction mem instr = match instr with
  | Pop x ->  (* POP: return top of stack, *then* increment ESP *)
    (* Move top of stack to address/register gop *)
	  let mem1 = M.interpret_instruction mem (Mov(x,top_stack)) in
	  (* Increment ESP by 4 -- Stack grows downwards *)
	  M.interpret_instruction mem1 (Arith(Add, (Reg ESP), (Imm 4L)))
  | Push x -> (* PUSH: decrement ESP, *then* store content *)
    (* Decrement ESP by 4 *)
  	let mem1 = M.interpret_instruction mem (Arith(Sub, (Reg ESP), (Imm 4L))) in
  	(* Move gop to top of stack *)
  	M.interpret_instruction mem1 (Mov(top_stack, x))
  | i -> M.interpret_instruction mem i

  let stackop mem operation gop = 
    match operation with 
      (* POP: return top, *then* increment ESP *)
      | Apop -> 
	(* Move top of stack to address/register gop *)
	let mem1 = M.interpret_instruction mem (Mov(gop, top_stack)) in
	(* Increment ESP by 4 -- Stack grows downwards *)
	M.interpret_instruction mem1 (Arith(Add, (Reg ESP), (Imm 4L)))
	  
      (* PUSH: decrement ESP, *then* store content *)
      | Apush ->
	(* Decrement ESP by 4 *)
	let mem1 = M.interpret_instruction mem (Arith (Sub, (Reg ESP), (Imm 4L))) in
	(* Move gop to top of stack *)
	M.interpret_instruction mem1 (Mov(top_stack, gop))
	 

   (* Notice: We push/pop offsets to the stack, not absolute addresses.*) 
  let call mem tgt ret =  
    (* push target address to stack *)
    let mem1 = stackop mem Apush (Imm (Int64.of_int ret))
    (* return list of possible call targets and their environments *)
    in get_vals mem1 tgt 
 
  let return mem = 
   (* Return top of stack and increment ESP by 4. We do not reuse
      the stackop function because POP stores its value in an
      op32 *)
    let mem1 = M.interpret_instruction mem (Arith (Add, (Reg ESP), (Imm 4L))) in
    get_vals mem1 (Address {  addrDisp = -4L;addrBase = Some ESP; addrIndex = None;segBase = None;})
      
     
     
  let print = M.print
  let print_delta = M.print_delta

  (* keep track of time *)
  let elapse = M.elapse

  let touch = M.touch 
  let set_value = M.set_value
end 


