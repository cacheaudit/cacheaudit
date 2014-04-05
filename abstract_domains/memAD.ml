let time_instr = 1 (* number of cycles of one instriction, to which we add the time for memory accesses *)
let time_test = 1 (* number of cycles for a test *)
let time_effective_load = 0; 

open X86Types
open AbstractInstr
open AD.DS
open NumAD.DS
open Logger

(** List of initial values for registers. Register * lower bound * upper bound *)
type reg_init_values = (X86Types.reg32 * int64 * int64) list

(** List of initial values for memory addresses. Adress * lower bound * upper bound *)
type mem_init_values = (int64 * int64 * int64) list

(** Parameters for the Memory Abstract Domain initialization *)
type mem_param = mem_init_values * reg_init_values

module type S =
  sig
    include AD.S
  
  (* init is used to return an initial abstract state *)
  (* the first arguments returns the initial value at a given address if it *)
  (* is defined, None otherwise (meaning it can be any) *)
  val init: (int64 -> int64 option) -> mem_param -> CacheAD.cache_param -> t

  (* from a genop32 expression, returns a finite list of possible values,
     each value associated with an approximation of the corresponding memory 
     states leading to that particular value. In case no finite list can be
     determied, returns Top.
  *)
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
          vals : F.t; (** Element of the Value Abstract Domain *)
          memory : MemSet.t; (** Set that contains the memory addresses used *)
          initial_values : int64 -> (int64 option); (** Function to determine
          the initial value of an address *)
          cache : C.t;
  }
  (** Main type for this module. *)

  let pp_vars fmt v = MemSet.iter (Format.fprintf fmt "%a, @," X86Print.pp_addr) v

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
      
  
      
  (** Type to differentiate memory reads from writes *)
  type rw_t = Read | Write


  (* Init functions for register map *)
  (* reg_to_var: X86Types.reg32 -> int64 *)
  (** Returns the variable number that corresponds to a given register *)
  let reg_to_var x = Int64.of_int (-(X86Util.reg32_to_int x) - 1)

  (** Gives variables a specific name if they have one (i.e. registers) *)
  let var_to_string v = match v with
    n -> try(
        X86Util.reg32_to_string (X86Util.int_to_reg32(-1-(Int64.to_int n)))
      ) with Invalid_argument "int_to_reg32" -> Format.sprintf "V%Lx" n

  (* initRegs : F.t -> F.t *)
  (** Adds variables and values for the registers in the value domain *)
  let initRegs v regList = 
    let varsadded = List.fold_left (fun vt x -> let r,_,_ = x in F.new_var vt (reg_to_var r)) v regList in
    List.fold_left (fun vt x -> let r,l,h = x in F.set_var vt (reg_to_var r) l h) varsadded regList

  (** Sets the initial values for certain addresses in the value domain *)
  let initVals v memList =
    let addresses_added = List.fold_left (fun vt x -> let var,_,_ = x in F.new_var vt var) v memList in
    List.fold_left (fun vt x -> let addr,l,h = x in F.set_var vt addr l h) addresses_added memList

  let initMemory m memList =
    List.fold_left (fun m x -> let addr,_,_ = x in MemSet.add addr m) m memList

 (** Return an element of type t with initialized registers *)
  let init iv (memList,regList) cache_params = {
    vals = initVals (initRegs (F.init var_to_string) regList) memList;
    memory = initMemory MemSet.empty memList;
    initial_values = iv;
    cache = C.init cache_params
  }


  (* merge_with : (F.t -> F.t -> F.t) -> t -> t -> t *)
  (** We create variables only present in one [t] and add them to the other [t]
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

  (** Join using the joins of the Flag and Cache abstract domains *)
  let join = merge_with F.join C.join
  
  (** Same as join *)
  let widen = merge_with F.widen C.widen

 (** Union of a sequence of abstract elements *)
 (* May raise Bottom *)
  let list_join = function
    [] -> raise Bottom
  | a::l -> List.fold_left join a l

  let subseteq x y = if x == y then true 
    else F.subseteq x.vals y.vals && MemSet.subset x.memory y.memory

  let test env cond =
    let lift = function Bot -> Bot 
    | Nb v -> Nb {env with vals = v; cache = C.elapse env.cache time_test} 
    in
    let (t,f) = F.test env.vals cond in
    lift t, lift f

  (* get_reg32 : t -> X86Types.reg32 -> (int64, F.t) finite_set *)
  (** @return the finite set of possible values or top corresponding to a register *)
  let get_reg32 env r = 
    F.get_var env.vals (reg_to_var r)

  exception Is_Top
  let unFinite = function
    | Nt x -> NumMap.bindings x
    | Tp -> raise Is_Top


  (* address_list : t -> X86Types.address -> (int64, F.t) list *)
  (** @return the list of all possible combinations resulting from the
      environment given an address. *)
  let address_list env addr =
    let base = match addr.addrBase with
                 Some reg ->  unFinite (get_reg32 env reg)
               | None -> [(0L, env.vals)] in
    assert(base<>[]);
    let index = match addr.addrIndex with
        Some(scale,reg) ->
          let intscale = Int64.of_int (X86Util.scale_to_size scale) in
          List.map (fun (x,e) -> (Int64.mul intscale x, e)) (unFinite (get_reg32 env reg))
      | None ->  [(0L, env.vals)] in
    (* Combine all the base and index values and meet the enviroments *)
    assert(index <> []);
    let comb = List.concat (List.map (fun (x,e) -> List.map (fun (y,e') -> (Int64.add x y, F.meet e e')) index) base) in
    List.map (fun (x, e) -> (Int64.add addr.addrDisp x, e)) comb
      
    
  (** Create an unitialized variable; assume it is not already created. *) 
  let create_var env n addr = {env with vals = F.new_var env.vals n; memory = MemSet.add n env.memory}

  (* var_of_op : t -> op32 -> rw_t -> (cons_var, t) list *)
  (** @return the list of constants or variables corresponding to the constant, register or address passed *)
  let var_of_op env gop rw = 
    match gop with
    | Imm x -> [(Cons x, NoMask, env)]
    | Reg r -> [(VarOp (reg_to_var r), NoMask, env)]
    | Address addr -> 
      try(
        let addrList = address_list env addr in
        assert(addrList<>[]);
        let read (n,e) = 
          let new_cache = 
            C.touch env.cache n in (* touch the cache on read and on write *)
            let (new_n,new_env) = match rw with
              | Read -> 
		let env = {env with vals =e} in
		if not (MemSet.mem n env.memory) then 
                  match env.initial_values n with
                      Some v -> Cons v, env
                    | None -> VarOp n, create_var env n n
		else VarOp n, env
              | Write -> 
              if not (MemSet.mem n env.memory) then 
                VarOp n,
                  let env = create_var env n n in
                  let env = {env with memory = MemSet.add n env.memory } in
                  match  env.initial_values n with
                    Some value -> 
                      {env with vals = F.update_val env.vals n NoMask (Cons value) NoMask Amov}
                    | None ->  env
              else (VarOp n, env) in
          new_n, NoMask, {new_env with cache = new_cache} in
        (* Get list of possible addresses and return either a var if it existed in the MemSet
        * or a cons (with the value) otherwise *)
          List.map read addrList) with Is_Top -> failwith "Top in a set of values referencing addresses, cannot continue"

  (** @return the 32-bit register that contains the given 8-bit register *)
  let r8_to_r32 r = X86Util.int_to_reg32 ((X86Util.reg8_to_int r) mod 4)

  (* var_of_op8 : t -> op8 -> rw_t -> (cons_var, mask_t, t) list *)
  (** Same as [var_of_op32] with the exception that we also return the position of the 8 bits in a 32-bit data as a mask *)
  let var_of_op8 env gop rw = match gop with
    | Imm x -> [(Cons x, Mask LL, env)]
    | Reg r -> 
        let address_mask = Mask (if X86Util.reg8_to_int r >= 4 then LH else LL) in
        [VarOp (reg_to_var (r8_to_r32 r)), address_mask, env]
    | Address addr -> try(
        let addrList = address_list env addr in
        let read (un,e) =
          let new_cache = C.touch env.cache un in
          (*let new_cache = match rw with Read -> TR.touch env.cache un 
                                      | Write -> env.cache in*)
          let n = Int64.logand un (Int64.lognot 3L) in
          let address_mask = rem_to_mask (Int64.logand (Int64.lognot un) 3L) in
          let (new_n, new_env) = match rw with
          | Read ->
              let env = {env with vals = e} in
              if not (MemSet.mem n env.memory) then
                match env.initial_values n with
                  Some v -> Cons v, env
                | None -> VarOp n, create_var env n n
              else VarOp n, env
          | Write ->
              if not (MemSet.mem n env.memory) then
                VarOp n,
                  let env = create_var env n n in
                  let env = {env with memory = MemSet.add n env.memory} in
                  match env.initial_values n with
                    Some value -> {env with vals = F.update_val env.vals n NoMask (Cons value) NoMask Amov}
                  | None -> env
              else (VarOp n, env) in
          new_n, Mask address_mask, {new_env with cache = new_cache}
        in
        List.map read addrList) with Is_Top -> failwith "Top in a set of values referencing addresses, cannot continue"

  (** return the enviroment that corresponds to a memory access *)
  let get_access_env d s ed es = 
    match d,s with
    | Address _, Address _ -> failwith "Memory-to-memory operation not supported" (* would currently record only one cache access *)
    | Address _, _ -> ed
    | _, Address _ -> es
    | _, _ -> ed

  (* For every possible value of src and dst, we do dst = dst op src using the value AD function update_val.
   * doOp, given a var or constant and an enviroment, generates a list of enviroments after updating each dst
   * with the var/cons given and the operation. *)
  let perform_op op dst dlist src slist =
    let do_op (s,smask,es) = List.map (fun (d,dmask,ed) -> 
      let access_env = (get_access_env dst src ed es) in
       { access_env with vals = 
        F.update_val access_env.vals (consvar_to_var d) dmask s smask op }
      ) dlist in
    list_join (List.concat (List.map do_op slist)) 
  
  
    (** Does the memory operation given by [AbstractInstr.memop] on the enviroment.
     This operation can be a move, an arithmetic operation or an exchange.
     Transmits the fact that time passes to the cache domain
     @return a new enviroment or raises a Bottom
     exception if the resulting environment is bottom. *)
  let memopfn m mop dst src v_o_op_dst v_o_op_src = try (
    let read_or_write = match mop with Aflag _ -> Read | _ -> Write in
    let dlist = v_o_op_dst m dst read_or_write in
    assert(dlist <> []);
    let slist = v_o_op_src m src Read in
    assert(slist <> []);
    let res = 
      match mop with
      | Aarith _ | Ashift _ | Amov | Aflag _ -> 
          perform_op mop dst dlist src slist
      | Aexchg -> 
          let s_to_d_vals = perform_op Amov dst dlist src slist in
          let d_to_s_vals = perform_op Amov src slist dst dlist in
          join s_to_d_vals d_to_s_vals
    in {res with cache = C.elapse res.cache time_instr}
  ) with Bottom -> failwith "MemAD.memop: Bottom in memAD after an operation on non bottom env"
  
  
  (** Performs the Load Effective Address instruction by loading each possible
    address in the variable correspoding to the register.
    @return an environment or raises Bottom *)
  let load_address env reg addr = let res = try( try (
    let addrList = address_list env addr in
    let regVar = reg_to_var reg in
    let envList = List.map (fun (x,e) -> { env with vals = F.update_val e regVar NoMask (Cons x) NoMask Amov }) addrList in
    list_join envList
  ) with Bottom -> failwith "MemAD.load_address: bottom after an operation on non bottom environment"
  ) with Is_Top -> let regVar = reg_to_var reg in {env with vals = F.set_var env.vals regVar 0L 0xffffffffL} 
  in {res with cache = C.elapse res.cache time_effective_load}
  
  
  (** Determines a finite list of possible values given a [genop32] with each
    value associated with the environment that led to it.
    @return the list mentioned above or Top if there is no finite list. *)
  (* in the resulting environments, time will have passed by an amount depending on the memory accesses *)
  let get_vals env gop = match gop with
    | Imm x -> Finite [(Int64.to_int x, env)]
    | Reg r -> 
        begin
          (* Get values correspoding to register r. If Top is returned, we return Top;
           * otherwise we return the values converted to int and its corresponding environment *)
          let vals = get_reg32 env r in
          match vals with
            Nt x -> Finite (List.map (fun (v,e) -> (Int64.to_int v, {env with vals = e})) (NumMap.bindings x))
          | Tp -> Top env
        end 
    | Address addr ->
        let addrList = address_list env addr in
        (* read - function for List.map *)
        let read (n, e) =
        begin
          if MemSet.mem n env.memory
          then
            (* if address is in MemSet, we get the values from F *)
            let vals = F.get_var e n in
            match vals with
              Nt x -> NumMap.bindings x
            | Tp -> raise Is_Top
          else 
            (* if address not in MemSet, we use the initilization function to get the value *)
            match env.initial_values n with
              Some value -> [value, e]
            | None -> raise Is_Top
        end
        in
        (* appendLists - function for List.fold_left; return the list with the contents of each address *)
        let appendLists r xs = (List.map (fun (x,e) -> (Int64.to_int x, {env with vals = e})) xs)@r
        in
        (try (
          let fsList = List.map read addrList in
          Finite (List.fold_left appendLists [] fsList)
        ) with Is_Top -> Top env)
  
  let memop env mop dst src = memopfn env mop dst src var_of_op var_of_op
  let memopb env mop dst src = memopfn env mop dst src var_of_op8 var_of_op8
  let movzx env dst32 src8 = memopfn env Amov dst32 src8 var_of_op var_of_op8
  let shift env op dst32 offset8 = memopfn env op dst32 offset8 var_of_op var_of_op8
  let flagop env fop dst src = memopfn env fop dst src var_of_op var_of_op
  
  let imul env dst src imm = failwith "IMUL not implemented"
  
  let rec interpret_instruction env instr = match instr with
  | Arith(op, x, y) -> begin
      match op with
        CmpOp -> interpret_instruction env (Cmp(x,y))
      | _ -> memop env (Aarith op) x y
    end
  | Arithb(op, x, y) -> begin
          match op with
            CmpOp -> failwith "8-bit CMP not implemented"
          | _ -> memopb env (Aarith op) x y
    end
  | Mov(x,y) -> memop env Amov x y
  | Movb(x,y) -> memopb env Amov x y
  | Exchange(x,y) -> memop env Aexchg (Reg x) (Reg y)
  | Movzx(x,y) -> movzx env x y
  | Lea(r,a) -> load_address env r a
  | Imul(dst,src,imm) -> imul env dst src imm
  | Shift(sop,x,y) -> shift env (Ashift sop) x y
  | Cmp(x, y) -> flagop env (Aflag Acmp) x y
  | Test(x, y) -> flagop env (Aflag Atest) x y
  | i -> Format.printf "@[Unexpected instruction %a @, in MemAD->interpret_instruction@]@." X86Print.pp_instr i;
    failwith ""
  
  
  (* we pass the elapsed time to the cache domain, the only one keeping track of it so far *)
  let elapse env d = {env with cache = C.elapse env.cache d}

  let touch env addr = {env with cache = C.touch env.cache addr}
        
end 

