open X86Types
open AsmUtil

(* Working example: ./cachecow -f Examples/aes_inlined --start 0x3b4 --inter --interverb fris *)

let verbose_instr = ref false
let verbose_regs = ref false
let verbose_flags = ref false
let ic = ref (-3)

(* Parameters *)
(* initial register values *)
let vals_flags = [(ZF, 0); (CF, 0)]

(* cache parameters *)
let cache_size = 16384
let line_size = 32
let associativity = 4

(* Initialize data structures *)
let memstack = VecStack.init ()
let regMap = Registers.init 8
let segMap = Registers.init 6
let flgMap = Flags.init 17
let cache = Cache.init cache_size line_size associativity


(* Values of registers at the start according to gdb *)
let load_values arr vals =
      List.iter (fun (r,v) -> Registers.set_value_32 arr r v) vals;
      VecStack.set_ebp memstack (List.assoc ESP vals)
let load_flags arr =
      List.iter (fun (f,v) -> Flags.set_flag arr f v) vals_flags

let calc_address addr = 
  let base = if Option.is_some addr.addrBase then Registers.get_value_32 regMap (Option.get addr.addrBase) else Int64.zero in
  let index = if Option.is_some addr.addrIndex
    then let scale,reg = Option.get addr.addrIndex in
         Int64.mul (Int64.of_int (X86Util.scale_to_size scale)) (Registers.get_value_32 regMap reg)
    else Int64.zero in
  let segment = if Option.is_some addr.segBase then Registers.get_segment segMap (Option.get addr.segBase) else Int64.zero in
  List.fold_right (fun x t -> Int64.add x t) [addr.addrDisp; base; index; segment] Int64.zero

let get_value_at_address f sections addr =
  let raddr = calc_address addr in
  Cache.write cache raddr;
  if VecStack.in_stack memstack raddr then
          f memstack (calc_address addr)
  else
          X86Headers.lookup sections raddr

let set_value_at_address f sections addr len value =
  let raddr = calc_address addr in
(*  Cache.write cache raddr;*)
  if VecStack.in_stack memstack raddr then
          (f memstack (calc_address addr) value; sections)
  else
          X86Headers.write sections raddr len value

(* 8-bit functions *)
(* Read a register, from memory or constant value *)
let get_genop8 sections gop = match gop with
  | Imm imm -> imm
  | Reg reg -> Registers.get_value_8 regMap reg
  | Address address -> Int64.logand (get_value_at_address VecStack.get_byte sections address) (0xFFL) (* Zero extend *)

(* Write in a register or to memory *)
let set_genop8 sections gop value =  match gop with
  | Imm imm -> failwith "Can't set constants"
  | Reg reg -> Registers.set_value_8 regMap reg value; sections
  | Address address -> set_value_at_address VecStack.set_byte sections address 8 value

(* 32-bit functions *)
(* Read a register, from memory or constant value *)
let get_genop32 sections gop = match gop with
  | Imm imm -> imm
  | Reg reg -> Registers.get_value_32 regMap reg
  | Address address -> get_value_at_address VecStack.get sections address

(* Write in a register or to memory *)
let set_genop32 sections gop value =  match gop with
  | Imm imm -> failwith "Can't set constants"
  | Reg reg -> Registers.set_value_32 regMap reg value;
               (match reg with
                | ESP -> VecStack.update_esp memstack value
                | _ -> ()
               ); sections
  | Address address -> set_value_at_address VecStack.set sections address 32 value

(* Test the flags against the condition passed *)
(*
Conditionals jumps used in the aes example are: JB/JAE - JNZ
We only have to update (for now) the CF and ZF flags
*)
let cond_jump cond truth =
  let f_val = if truth then 1 else 0 in
  match cond with
    O -> Flags.get_flag flgMap OF = f_val (* O/NO *)
  | B -> Flags.get_flag flgMap CF = f_val (* B/AE *)
  | Z -> Flags.get_flag flgMap ZF = f_val (* Z/NZ *)
  | BE -> let boolop = if truth then (||) else (&&) in (* BE/A *)
          boolop (Flags.get_flag flgMap CF = f_val) (Flags.get_flag flgMap ZF = f_val)
  | S -> let flg = if truth then SF else ZF in (* S/NE *)
         Flags.get_flag flgMap flg = f_val
  | P -> Flags.get_flag flgMap PF = f_val (* P/NP *)
  | L -> let cmp = if truth then (<>) else (=) in (* L/GE *)
         cmp (Flags.get_flag flgMap SF) (Flags.get_flag flgMap OF)
  | LE -> let boolop = if truth then (||) else (&&) in  (* LE/G *)
          let cmp = if truth then (<>) else (=) in
          boolop (Flags.get_flag flgMap ZF = f_val) (cmp (Flags.get_flag flgMap SF) (Flags.get_flag flgMap OF))

(* Perform the arithmetic operation given by aop *)
exception OperationExn of string
let arith_operation aop src dst =
  match aop with
    Add -> Int64.add dst src
  | Addc -> Int64.add dst (Int64.add src (Int64.of_int (Flags.get_flag flgMap CF)))
  | And -> Int64.logand dst src
  | CmpOp -> Int64.sub dst src
  | Or -> Int64.logor dst src
  | Sub -> Int64.sub dst src
  | Subb ->  Int64.sub dst (Int64.add src (Int64.of_int (Flags.get_flag flgMap CF)))
  | Xor -> Int64.logxor dst src
  | _ -> raise (OperationExn "interpreter: Unsupported arithmetic operation")

(* Update the Zero Flag *)
let update_flag_Z res =
  let value = if res = Int64.zero then 1 else 0 in
  Flags.set_flag flgMap ZF value

(* Update the Carry Flag *)
let update_flag_C res size =
(*   (*new version*)
  let upper_bound = Int64.zero in (*Int64.shift_right Int64.one size in*)
  let lower_bound = Int64.shift_left (Int64.neg Int64.zero) size in
  let value = if res < upper_bound && res >= lower_bound then 0 else 1 in
  Flags.set_flag flgMap CF value*)
  
  (*old version*)
  let upper_bound = Int64.shift_left Int64.one size in
  let lower_bound = 0L in
  let value = if res < lower_bound || res >= upper_bound then 1 else 0 in
  Flags.set_flag flgMap CF value

let update_flag_C_shift carry = let value = if carry <> Int64.zero then 1 else 0 in
                                Flags.set_flag flgMap CF value

(* Interprets the current instruction and increments the PC accordingly *)
let rec steps sections bits = 
  if more bits then
    let this_addr = get_byte bits in
    let instr,new_bits = X86Parse.read_instr bits in
      if !verbose_regs then Registers.print_regs regMap;
      if !verbose_flags then Flags.print_flags flgMap;
      if !verbose_instr then Format.printf "@<6>%d\t0x%X\t%a@." !ic this_addr X86Print.pp_instr instr;
      ic := !ic + 1; (* Update instruction counter *)
      match instr with
        | Arith (aop, dst, src) ->
          let sval = get_genop32 sections src in
          let dval = get_genop32 sections dst in
          let res = arith_operation aop sval dval in
          let new_sec = (match aop with
            CmpOp -> sections (* Cmp doesn't store the result; only updates flags *)
            | _ -> set_genop32 sections dst res) in
          (* Update flags *)
          update_flag_Z res;
          update_flag_C res 32;
          steps new_sec new_bits
        | Arithb (aop, dst, src) -> 
          let sval = get_genop8 sections src in
          let dval = get_genop8 sections dst in
          let res = arith_operation aop sval dval in
          let new_sec = (match aop with
            CmpOp -> sections
          | _ -> set_genop8 sections dst res) in
          (* Update flags *)
          update_flag_Z res;
          update_flag_C res 8;
          steps new_sec new_bits
        | Call dst -> (* TODO *)
          let jmp = Int64.to_int (get_genop32 sections dst) in
          steps sections (goto bits jmp)
        | Cmp (dst, src) -> 
          let sval = get_genop32 sections src in
          let dval = get_genop32 sections dst in
          let res =  Int64.sub dval sval in
          (* Update flags *)
          update_flag_Z res;
          update_flag_C res 32;
          steps sections new_bits
        | Test (dst, src) ->
          let sval = get_genop32 sections src in
          let dval = get_genop32 sections dst in
          let res =  Int64.logand dval sval in
          (* Update flags *)
          update_flag_Z res;
          update_flag_C res 32;
          steps sections new_bits
        | Inc gop ->
          let value = get_genop32 sections gop in
          let res = Int64.add value Int64.one in
          let new_sec = set_genop32 sections gop res in
          (* Update flags *)
          update_flag_Z res;
          update_flag_C res 32;
          steps new_sec new_bits
        | Dec gop ->
          let value = get_genop32 sections gop in
          let res = Int64.sub value Int64.one in
          let new_sec = set_genop32 sections gop res in
          (* Update flags *)
          update_flag_Z res;
          update_flag_C res 32;
          steps new_sec new_bits
        | Jcc (cc, imm) ->
          let truth,cond = cc in
          let jmpbits = if cond_jump cond truth
            then goto bits (Int64.to_int imm)
            else new_bits in
          steps sections jmpbits
        | Jmp gop ->
          let jmp = Int64.to_int (get_genop32 sections gop) in
	  steps sections (goto bits jmp)
        | Lea (dst, src) ->
          let addr = calc_address src in
          let new_sec = set_genop32 sections (Reg dst) addr in
          steps new_sec new_bits
        | Leave ->
          (* MOV ESP, EBP *)
          let sval = get_genop32 sections (Reg EBP) in
          let new_sec = set_genop32 sections (Reg ESP) sval in
          (* POP EBP *)
          let top = VecStack.pop memstack in
          let new_sec_2 = set_genop32 new_sec (Reg EBP) top in
          (* Get old ESP and calculate new ESP by adding 4 to it (stack shrinks) *)
          let old_esp = Registers.get_value_32 regMap ESP in
          let new_esp = Int64.add old_esp 4L in
          (* Set new ESP value *)
          let new_sec_3 = set_genop32 new_sec_2 (Reg ESP) new_esp in
          Cache.write cache old_esp;
          steps new_sec_3 new_bits
        | Mov (dst, src) ->
          let sval = get_genop32 sections src in
          let new_sec = set_genop32 sections dst sval in
          steps new_sec new_bits
        | Movb (dst, src) ->
          let sval = get_genop8 sections src in
          let new_sec = set_genop8 sections dst sval in
          steps new_sec new_bits
        | Movzx (dst, src) -> (* Zero extend is done by writing the obtained value as a 32-bit register *)
          let sval = get_genop8 sections src in
          let new_sec = set_genop32 sections dst sval in
          steps new_sec new_bits
        | Exchange (r1, r2) ->
          let tmp = Registers.get_value_32 regMap r1 in
          Registers.set_value_32 regMap r1 (Registers.get_value_32 regMap r2);
          Registers.set_value_32 regMap r2 tmp;
          steps sections new_bits
        | Pop gop ->
            (* ESP points to last element used, not first free space;
             * need to check semantics of pop and push *)
          (* Get value from the top of the stack *)
          let top = VecStack.pop memstack in
          (* Set the value in a register or memory *)
          let new_sec = set_genop32 sections gop top in
          (* Get old ESP and calculate new ESP by adding 4 to it (stack shrinks) *)
          let old_esp = Registers.get_value_32 regMap ESP in
          let new_esp = Int64.add old_esp 4L in
          (* Set new ESP value *)
          let new_sec_2 = set_genop32 new_sec (Reg ESP) new_esp in
          (* Write to cache the stack address accessed *)
          Cache.write cache old_esp;
          steps new_sec_2 new_bits
        | Push gop ->
          (* Push value onto the stack *)
          VecStack.push memstack (get_genop32 sections gop);
          (* Get old ESP and calculate new ESP (stack grows) *)
          let new_esp = Int64.sub (Registers.get_value_32 regMap ESP) 4L in
          let new_sec = set_genop32 sections (Reg ESP) new_esp in
          (* Write to cache the stack address accessed *)
          Cache.write cache new_esp;
          steps new_sec new_bits
        | Ret -> ()
        | Shift (sop, dst, offset) -> (* Update flags *)
          let dval = get_genop32 sections dst in
          let soff = Int64.to_int (get_genop8 sections offset) in
          (* TODO: add ROR / ROL *)
          let new_val = (
           match sop with
             Shl -> let shifted_value = Int64.shift_left dval soff in
                    let carry = Int64.logand shifted_value (Int64.shift_left Int64.one 32) in
                    update_flag_C_shift carry; shifted_value
           | Shr -> let mask = Int64.shift_left Int64.one (soff - 1) in
                    let carry = Int64.logand mask dval in
                    update_flag_C_shift carry;
                    Int64.shift_right_logical dval soff
           | Sar -> let mask = Int64.shift_left Int64.one (soff - 1) in
                    let carry = Int64.logand mask dval in
                    let value_32 = Int64.to_int32 dval in
                    update_flag_C_shift carry;
                    Int64.of_int32 (Int32.shift_right value_32 soff)
          | _ -> raise (OperationExn "interpreter: Unsupported shift operation")
          )
          in let new_sec = set_genop32 sections dst new_val in
          update_flag_Z new_val;
          steps new_sec new_bits
        | Halt -> steps sections new_bits
        | Skip -> steps sections new_bits
        | FlagSet (flg, truth) -> 
          Flags.set_flag flgMap flg (if truth then 1 else 0);
          steps sections new_bits
        | _ -> raise (OperationExn "interpreter: Unsupported operation")
  else ()

let interpret file sections start_addr start_values varray = 
  let bit_file = goto (read_from_file file) start_addr in
  (* Set verbose options 
   * 0 - instructions
   * 1 - registers
   * 2 - flags
   * 3 - stack
   * 4 - cache *)
  verbose_instr := varray.(0); verbose_regs := varray.(1); verbose_flags := varray.(2);
  VecStack.set_verbose memstack varray.(3); Cache.set_verbose cache varray.(4);
  (* Load register and flag values *)
  load_values regMap start_values;
  load_flags flgMap;
  (* Start interpretation *)
  steps sections bit_file;
  (* Print final flag and register values *) 
  if !verbose_regs then Registers.print_regs regMap;
  if !verbose_flags then Flags.print_flags flgMap
