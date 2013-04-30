open Signatures
open X86Types

(* Simple stack abstract domain. Translates pushs and pops to register operations, but does not yet support function calls *)

module StackAD (M: MEMORY_ABSTRACT_DOMAIN) : CALL_ABSTRACT_DOMAIN = struct
    (* Stack.top and Memory abstract domain *)
  type t = M.t

   (* Stackbase addresses hardwired, taken from interpreter *)
  let init mem cache_params = M.init (fun addr -> 
    if addr=Int64.zero then failwith "0 address raises seg fault \n"
    else try Some(X86Headers.lookup mem addr) with 
        (*We then assume it is not initialzed*)
      X86Headers.InvalidVirtualAddress -> None) cache_params (*TDO check that it falls in the stack *)
  let join = M.join 
  let widen = M.widen 
  let subseteq = M.subseteq 
  let get_offset  = M.get_offset
  let test = M.test
 
      
  (* Check with Laurent if Op is needed *)
  let memop = M.memop
  let memopb  = M.memopb
  let movzx = M.movzx
  let flagop = M.flagop
  let load_address = M.load_address
  let shift = M.shift
  let top_stack =  Address {  addrDisp = 0L;
                              addrBase = Some ESP;
                              addrIndex = None;
                              segBase = None;}
  let stackop mem operation gop = 
    match operation with 
      (* POP: return top, *then* increment ESP *)
      | ADpop -> 
	(* Move top of stack to address/register gop *)
	let mem1 = memop mem ADmov gop top_stack in
	(* Increment ESP by 4 -- Stack grows downwards *)
	memop mem1 (ADarith Add) (Reg ESP) (Imm 4L)
	  
      (* PUSH: decrement ESP, *then* store content *)
      | ADpush ->
	(* Decrement ESP by 4 *)
	let mem1 = memop mem (ADarith Sub) (Reg ESP) (Imm 4L) in
	(* Move gop to top of stack *)
	memop mem1 ADmov top_stack gop
	 

   (* Notice: We push/pop offsets to the stack, not absolute addresses.*) 
  let call mem tgt ret =  
    (* push target address to stack *)
    let mem1 = stackop mem ADpush (Imm (Int64.of_int ret))
    (* return list of possible call targets and their environments *)
    in get_offset mem1 tgt 
 
  let return mem = 
   (* Return top of stack and increment ESP by 4. We do not reuse
      the stackop function because POP stores its value in an
      op32 *)
    let mem1 = memop mem (ADarith Add) (Reg ESP) (Imm 4L) in
    get_offset mem1 (Address {  addrDisp = -4L;addrBase = Some ESP; addrIndex = None;segBase = None;})
      
     
     
  let print = M.print
  let print_delta = M.print_delta

  (* keep track of time *)
  let elapse = M.elapse
end 


