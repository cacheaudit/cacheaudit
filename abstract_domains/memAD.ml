open X86Types
open AbstractInstr
open AD.DS
open NumAD.DS
open Logger

(* times added to the time for memory accesses *)
let time_instr = 1 (* number of cycles of one instriction *)
let time_test = 1 (* number of cycles for a test *)
let time_effective_load = 0; 

(* Operand type, which may be a 8 or 32-bit operand*)
type op_t = Op8 of X86Types.op8 | Op32 of X86Types.op32

(* Type to differentiate memory reads from writes *)
type rw_t = Read | Write

(* List of initial values for registers *)
type reg_init_values = (X86Types.reg32 * int64 * int64) list

(* List of initial values for memory addresses  *)
type mem_init_values = (int64 * int64 * int64) list

(* Parameters for the Memory Abstract Domain initialization *)
type mem_param = mem_init_values * reg_init_values

module type S =
  sig
    include AD.S
  val init: (int64 -> int64 option) -> mem_param -> CacheAD.cache_param -> t
  val get_vals: t -> op32 -> (int,t) finite_set
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  val interpret_instruction : t -> X86Types.instr -> t
  val touch : t -> int64 -> t
  val elapse : t -> int -> t
end
  


let logged_addresses = ref []

let log_address addr =
  logged_addresses := !logged_addresses @ [addr]

module MemSet = Set.Make(Int64)

(* *)
module Make (F : FlagAD.S) (C:CacheAD.S) = struct

  type t = {
          vals : F.t; (* storing values of variables *)
          memory : MemSet.t; (* set storing the memory addresses used *)
          initial_values : int64 -> (int64 option); 
          cache : C.t;
  }
  
  (*** Printing and logging ***)

  let pp_vars fmt v = 
    MemSet.iter (Format.fprintf fmt "%a, @," X86Print.pp_addr) v

  let log_vars mem = List.iter (F.log_var mem.vals) !logged_addresses

  let print fmt mem = 
    if MemSet.is_empty mem.memory then Format.fprintf fmt "%a %a"
        F.print mem.vals C.print mem.cache
    else
      log_vars mem;
      if get_log_level MemLL <> Quiet then
        Format.fprintf fmt "@;List of variable memory locations:@;  @[%a@;%a@, %a@]"
        pp_vars mem.memory F.print mem.vals C.print mem.cache
      else 
        C.print fmt mem.cache

  let print_delta mem1 fmt mem2 = 
    Format.fprintf fmt "@[";
    if get_log_level MemLL = Debug then begin
      let added_vars = MemSet.diff mem2.memory mem1.memory
      and removed_vars = MemSet.diff mem1.memory mem2.memory in
      if not(MemSet.is_empty added_vars) then
        Format.fprintf fmt "Added variables %a@," pp_vars added_vars;
      if not(MemSet.is_empty added_vars) then
        Format.fprintf fmt "Removed variables %a@," pp_vars removed_vars;
    end;
    Format.fprintf fmt "%a @, %a@]"
      (F.print_delta mem1.vals) mem2.vals
      (C.print_delta mem1.cache) mem2.cache
      
  (*** Initialization ***)
  
  (* Returns the variable number that corresponds to a given register *)
  let reg_to_var x = Int64.of_int (-(X86Util.reg32_to_int x) - 1)

  (* Gives variables a specific name if they have one (i.e. registers) *)
  let var_to_string v = match v with
    n -> try(
        X86Util.reg32_to_string (X86Util.int_to_reg32(-1-(Int64.to_int n)))
      ) with Invalid_argument "int_to_reg32" -> Format.sprintf "V%Lx" n

  (* Adds variables and values for the registers in the value domain *)
  let initRegs v regList = 
    let varsadded = List.fold_left (fun vt x -> 
      let r,_,_ = x in F.new_var vt (reg_to_var r)) v regList in
    List.fold_left (fun vt x -> 
      let r,l,h = x in F.set_var vt (reg_to_var r) l h) varsadded regList

  (* Sets the initial values for certain addresses in the value domain *)
  let initVals v memList =
    let addresses_added = List.fold_left (fun vt x -> 
      let var,_,_ = x in F.new_var vt var) v memList in
    List.fold_left (fun vt x -> 
      let addr,l,h = x in F.set_var vt addr l h) addresses_added memList

  let initMemory m memList =
    List.fold_left (fun m x -> let addr,_,_ = x in MemSet.add addr m) m memList

 (* Return an element of type t with initialized registers *)
  let init iv (memList,regList) cache_params = {
    vals = initVals (initRegs (F.init var_to_string) regList) memList;
    memory = initMemory MemSet.empty memList;
    initial_values = iv;
    cache = C.init cache_params
  }

  
  (*** Functions for abstract interpretation ***)
  
  (* We create variables only present in one [t] and add them to the other [t]
    and vice versa, in order to apply "merge" function and combine the two
    environments. *)
  let merge_with f g x y = if x == y then x else
    let x_minus_y = MemSet.diff x.memory y.memory in
    let y_minus_x = MemSet.diff y.memory x.memory in
    let create_vars = MemSet.fold (fun x v -> F.new_var v x) in
    { x with vals = f (create_vars y_minus_x x.vals) 
                      (create_vars x_minus_y y.vals);
             memory = MemSet.union x.memory y.memory;
             cache = g x.cache y.cache
    }

  (* Join using the joins of the Flag and Cache abstract domains *)
  let join = merge_with F.join C.join
  
  (* Same as join *)
  let widen = merge_with F.widen C.widen

 (* Union of a sequence of abstract elements *)
 (* May raise Bottom *)
  let list_join = function
    [] -> raise Bottom
  | a::l -> List.fold_left join a l

  let subseteq x y = if x == y then true 
    else F.subseteq x.vals y.vals && MemSet.subset x.memory y.memory
  
  
  (*** Functions for interpreting instructions ***)
  
  (* returns the finite set of possible values or top corresponding to a register *)
  let get_reg32 env r = 
    F.get_var env.vals (reg_to_var r)

  exception Is_Top
  let unFinite = function
    | Nt x -> NumMap.bindings x
    | Tp -> raise Is_Top

  (* Returns a list of all possible addresses and corresponding value environment *)
  (* which are described by addr. The resulting addresses are possible evaluations *)
  (* of base + index*scale + disp *)
  let get_addresses env addr =
    let base = match addr.addrBase with
                 Some reg ->  unFinite (get_reg32 env reg)
               | None -> [(0L, env.vals)] in
    assert(base<>[]);
    let index = match addr.addrIndex with
        Some(scale,reg) ->
          let intscale = Int64.of_int (X86Util.scale_to_size scale) in
          List.map (fun (x,e) -> 
            (Int64.mul intscale x, e)) (unFinite (get_reg32 env reg))
      | None ->  [(0L, env.vals)] in
    (* Combine all the base and index values and meet the enviroments *)
    assert(index <> []);
    let comb = List.concat (List.map (fun (x,e) -> List.map (fun (y,e') -> 
      (Int64.add x y, F.meet e e')) index) base) in
    List.map (fun (x, e) -> (Int64.add addr.addrDisp x, e)) comb
      
    
  (* Create an unitialized variable; assume it is not already created. *) 
  let create_var env n addr = 
    {env with vals = F.new_var env.vals n; memory = MemSet.add n env.memory}
  
  (* return the 32-bit register that contains the given 8-bit register *)
  let r8_to_r32 r = X86Util.int_to_reg32 ((X86Util.reg8_to_int r) mod 4)
  
  let op_to_op32 op = match op with
    | Op32(Imm x) | Op8(Imm x) -> Imm x
    | Op32(Reg r) -> Reg r
    | Op8(Reg r) -> Reg (r8_to_r32 r)
    | Op32(Address addr) | Op8(Address addr) -> Address addr
  
  (* Give a list of constants or variables corresponding to the an operand. *)
  (* If the operand is not 32 bit, also a mask is returned which tells *)
  (* which bits of the 32 are to be used.*)
  (* If memory is accessed, the CacheAD is notified, and if a new location *)
  (* is written, a new variable is created.*)
  let get_consvars env operand rw = 
    let is_op32 = match operand with
    | Op8(_) -> false | Op32(_) -> true in
    let op = op_to_op32 operand in
    match op with
    | Imm x -> 
      let mask = if is_op32 then NoMask else Mask LL in
      [(Cons x, mask, env)]
    | Reg r -> 
      let mask = match operand with
        | Op8(Reg r) -> Mask (if X86Util.reg8_to_int r >= 4 then LH else LL) 
        | Op32(Reg _) -> NoMask | _ -> assert false in
      [(VarOp (reg_to_var r), mask, env)]
    | Address addr -> 
      try(
        let get_a_mask a = 
          if is_op32 then a,NoMask 
          else
            let new_a = Int64.logand a (Int64.lognot 3L) in
            let mask = Mask (rem_to_mask (Int64.logand (Int64.lognot a) 3L)) in
            new_a, mask in
        let access_addr (a,avals) = 
          let new_cache = C.touch env.cache a in
          let a,mask = get_a_mask a in
          (* Update the values to environment corresponding to a (which excludes*)
          (* values connected to other possible evaluations of a).*)
          (* In older version was only performed on Read, now it is moved here *)
          (* as I don't see why this shouldn't be done at a write. *)
          let env = {env with vals = avals} in
          let new_a,new_env = match rw with
          | Read ->
              if MemSet.mem a env.memory then VarOp a, env
              else begin
                match env.initial_values a with
                (* when we only read variable with initial value, there is*)
                (*  no need to maintain  it in memory, treat it as a constant *)
                | Some v -> Cons v, env 
                | None -> VarOp a, create_var env a a
              end
          | Write -> let env =
              if MemSet.mem a env.memory then env
              else
                let env = create_var env a a in
                  match  env.initial_values a with
                  | Some value ->  
                    (* we will be changing the content of the variable, so make*)
                    (* sure the initial value is in memory before that *)
                    {env with vals = 
                      F.update_val env.vals a NoMask (Cons value) NoMask Amov}
                  | None ->  env in
            VarOp a, env in
          new_a, mask, {new_env with cache = new_cache} in
        (* Get list of possible addresses, simulate access to all of them and *)
        (* return the list of corresponding variables and environments *)
        let addrList = get_addresses env addr in
        assert(addrList<>[]);
        List.map access_addr addrList
      ) with Is_Top -> 
        failwith "Top in a set of values referencing addresses, cannot continue"

  (* return the enviroment that corresponds to a memory access *)
  let get_access_env d s ed es = 
    match d,s with
    | Op32(Address _), Op32(Address _) | Op8(Address _), Op8(Address _) ->
      failwith "Memory-to-memory operation not supported" 
      (* would currently record only one cache access *)
    | Op32(Address _), _ | Op8(Address _), _ -> ed
    | _, Op32(Address _) | _, Op8(Address _) -> es
    | _, _ -> ed
  
  let is_reg op = match op with
  | Op32(Reg _) | Op8(Reg _) -> true
  | _ -> false
  
  (* Updates the memory according to the actions perscribed by op; *)
  (* get the possible sources and destitnations and pass further changes *)
  (* to FlagAD and CacheAD *)
  let update_mem env op dst src op3 = try (
    if op <> Aimul then assert (op3 = None);
    let read_or_write = match op with Aflag _ -> Read | _ -> Write in
    let dlist = get_consvars env dst read_or_write in
    assert(dlist <> []);
    let slist = get_consvars env src Read in
    assert(slist <> []);
    (* For every possible value of src and dst, do dst = dst op src, updating *)
    (* the values by passing the operation to update_val from FlagAD. *)
    let perform_op op dst dlist src slist =
      let do_op (s,smask,es) = List.map (fun (d,dmask,ed) -> 
        let access_env = (get_access_env dst src ed es) in
         { access_env with vals = 
          F.update_val access_env.vals (consvar_to_var d) dmask s smask op }
        ) dlist in
      list_join (List.concat (List.map do_op slist)) in
    let res = 
      match op with
      | Aarith _ | Ashift _ | Amov | Aflag _ -> 
          perform_op op dst dlist src slist
      | Aexchg -> 
          let s_to_d_vals = perform_op Amov dst dlist src slist in
          let d_to_s_vals = perform_op Amov src slist dst dlist in
          join s_to_d_vals d_to_s_vals
      | Aimul ->
        let src2,imm = match op3 with
        | Some x -> begin 
            match x with 
            | (Op32(Imm i)) -> x,i 
            | _ -> failwith "Only 32-bit immediates supported for imul"
          end
        | None -> failwith "2-operand imul not implemented" in
        (* first move immediate to dst, then do dst = src * dst *)
        (* to extend this to 2-operand imul, skip the first step *)
        let env = perform_op Amov dst dlist src2 [(Cons imm,NoMask,env)] in
        assert (is_reg dst); (* make sure we are accessing a register, because
        if it were the memory, this would record a second access to the cache *)
        let dlist = get_consvars env dst Read in
        perform_op Aimul dst dlist src slist in
     {res with cache = C.elapse res.cache time_instr}
  ) with Bottom -> failwith 
    "MemAD.update_mem: Bottom in memAD after an operation on non bottom env"
  
  
  (* Performs the Load Effective Address instruction by loading each possible
    address in the variable correspoding to the register.
    @return an environment or raises Bottom *)
  let load_address env reg addr = let res = try( try (
      let addrList = get_addresses env addr in
      let regVar = reg_to_var reg in
      let envList = List.map (fun (x,e) -> { env with vals = 
        F.update_val e regVar NoMask (Cons x) NoMask Amov }) addrList in
      list_join envList
    ) with Bottom -> failwith 
      "MemAD.load_address: bottom after an operation on non bottom environment"
    ) with Is_Top -> let regVar = reg_to_var reg in 
      {env with vals = F.set_var env.vals regVar 0L 0xffffffffL} 
    in {res with cache = C.elapse res.cache time_effective_load}
  
  
  (*** Public functions ***)
  
  let rec interpret_instruction env instr = match instr with
  | Arith(op, dst, src) -> begin
      match op with
        CmpOp -> interpret_instruction env (Cmp(dst,src))
      | _ -> update_mem env (Aarith op) (Op32 dst) (Op32 src) None
    end
  | Arithb(op, dst, src) -> begin
          match op with
            CmpOp -> failwith "8-bit CMP not implemented"
          | _ -> update_mem env (Aarith op) (Op8 dst) (Op8 src) None
    end
  | Mov(dst,src) -> update_mem env Amov (Op32 dst) (Op32 src) None
  | Movb(dst,src) -> update_mem env Amov (Op8 dst) (Op8 src) None
  | Exchange(dst,src) -> 
    update_mem env Aexchg (Op32 (Reg dst)) (Op32 (Reg src)) None
  | Movzx(dst32,src8) -> update_mem env Amov (Op32 dst32) (Op8 src8) None
  | Lea(r,a) -> load_address env r a
  | Imul(dst,src,imm) -> 
    update_mem env Aimul (Op32 (Reg dst)) (Op32 src) (Some (Op32 (Imm imm)))
  | Shift(sop,dst32,offst8) -> 
    update_mem env (Ashift sop) (Op32 dst32) (Op8 offst8) None
  | Cmp(dst, src) -> update_mem env (Aflag Acmp) (Op32 dst) (Op32 src) None
  | Test(dst, src) -> update_mem env (Aflag Atest) (Op32 dst) (Op32 src) None
  | i -> Format.printf "@[Unexpected instruction %a @, 
    in MemAD->interpret_instruction@]@." X86Print.pp_instr i;
    failwith ""
  
  let get_vals env gop = match gop with
    | Imm x -> Finite [(Int64.to_int x, env)]
    | Reg r -> 
        begin
          (* Get values correspoding to register r. May return Top or
             the values converted to int and their corresponding environments *)
          let vals = get_reg32 env r in
          match vals with
            Nt x -> Finite (List.map (fun (v,e) -> 
              (Int64.to_int v, {env with vals = e})) (NumMap.bindings x))
          | Tp -> Top env
        end 
    | Address addr ->
        let addrList = get_addresses env addr in
        (* read - function for List.map *)
        let read (n, e) =
        begin
          if MemSet.mem n env.memory
          then
            let vals = F.get_var e n in
            match vals with
              Nt x -> NumMap.bindings x
            | Tp -> raise Is_Top
          else 
            match env.initial_values n with
              Some value -> [value, e]
            | None -> raise Is_Top
        end
        in
        (* appendLists - function for List.fold_left; *)
        (* return the list with the contents of each address *)
        let appendLists r xs = (List.map (fun (x,e) -> 
          (Int64.to_int x, {env with vals = e})) xs)@r
        in
        (try (
          let fsList = List.map read addrList in
          Finite (List.fold_left appendLists [] fsList)
        ) with Is_Top -> Top env)
        
  let test env cond =
    let lift = function Bot -> Bot 
    | Nb v -> Nb {env with vals = v; cache = C.elapse env.cache time_test} 
    in
    let (t,f) = F.test env.vals cond in
    lift t, lift f
  
  (* pass the elapsed time to the cache domain which keeps  track of it *)
  let elapse env d = {env with cache = C.elapse env.cache d}

  let touch env addr = {env with cache = C.touch env.cache addr}
        
end 

