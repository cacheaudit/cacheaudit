open Signatures
open X86Types

(* Architecture abstract domain. Right now it only allows two different caches for instructions and data *)

let instruction_addr_base = ref (Int64.of_int 0)

module SplitCacheArchitectureAD (C: CALL_ABSTRACT_DOMAIN) (IC: CACHE_ABSTRACT_DOMAIN) : ARCHITECTURE_ABSTRACT_DOMAIN = struct

  type t = {
    call_ad: C.t;
    inst_ad: IC.t
  }

  let init concr_mem start_values data_cache_params inst_cache_params addr_base = 
    instruction_addr_base := addr_base;
    {
      call_ad = C.init concr_mem start_values data_cache_params;
      inst_ad = IC.init (match inst_cache_params with
          Some(params) -> params
        | _ -> failwith "No/Invalid parameters supplied to instruction cache")
    }

  let subs_e env call_env = {env with call_ad=call_env}
  let subs_finite_set env fs = match fs with
    | Top(v) -> Top(subs_e env v)
    | Finite(l) -> Finite (List.map (fun (n,e) -> (n,subs_e env e)) l)

  
  let join env env2 = 
    let call_ad = C.join env.call_ad env2.call_ad in
    let inst_ad = IC.join env.inst_ad env2.inst_ad in
    {call_ad = call_ad; inst_ad = inst_ad}
  
  let widen env env2 =
    let call_ad = C.widen env.call_ad env2.call_ad in
    let inst_ad = IC.widen env.inst_ad env2.inst_ad in
    {call_ad = call_ad; inst_ad = inst_ad}

  let subseteq env env2 = C.subseteq env.call_ad env2.call_ad && IC.subseteq env.inst_ad env2.inst_ad

  let test env cond = 
    let subs_nb = function
    | Bot -> Bot
    | Nb(v) -> Nb(subs_e env v) in
    let (l,r) = (C.test env.call_ad cond) in
    (subs_nb l,subs_nb r)

  (* Redirect all usual stack calls to the stackAD *)
  let get_offset env op = subs_finite_set env (C.get_offset env.call_ad op)
  let memop env mop op1 op2 = subs_e env (C.memop env.call_ad mop op1 op2)
  let memopb  env mop op1 op2 = subs_e env (C.memopb env.call_ad mop op1 op2)
  let movzx env op1 op2 = subs_e env (C.movzx env.call_ad op1 op2)
  let flagop env fop = subs_e env (C.flagop env.call_ad fop)
  let load_address env reg add = subs_e env (C.load_address env.call_ad reg add)
  let shift env sop op1 op2 = subs_e env (C.shift env.call_ad sop op1 op2)
  let stackop env sop op1 = subs_e env (C.stackop env.call_ad sop op1) 
  let call env op n = subs_finite_set env (C.call env.call_ad op n)
  let return env = subs_finite_set env (C.return env.call_ad)
  let print form env = 
    Printf.printf "\n\n\n\n#######################\n\n\n------ Data Cache -----\n\n";
    C.print form env.call_ad;
    Printf.printf "\n\n\n-- Instruction Cache --\n";
    IC.print form env.inst_ad;
    Printf.printf "\n-----------------------\n\n"

  let print_delta env1 form env2 = 
    Printf.printf "\n\n\n\n#######################\n\n\n------ Data Cache -----\n\n";
    C.print_delta env1.call_ad form env2.call_ad;
    Printf.printf "\n\n\n-- Instruction Cache --\n\n";
    IC.print_delta env1.inst_ad form env2.inst_ad;
    Printf.printf "\n-----------------------\n\n"

  let elapse env t = {
    call_ad = C.elapse env.call_ad t;
    inst_ad = IC.elapse env.inst_ad t
  }

  let read_instruction env addr = { env with inst_ad = (IC.touch env.inst_ad (Int64.add (Int64.of_int addr) !instruction_addr_base)) }
  (* Instructions are isolated from data so it doesn't matter if the address is relative *)

end

module JointCacheArchitectureAD (C: CALL_ABSTRACT_DOMAIN) : ARCHITECTURE_ABSTRACT_DOMAIN = struct

  type t = C.t

  let init concr_mem start_values data_cache_params inst_cache_params addr_base =
    instruction_addr_base := addr_base;
    C.init concr_mem start_values data_cache_params

  (* Redirect all usual stack calls to the stackAD *)
  let join = C.join
  let widen = C.widen
  let subseteq = C.subseteq
  let get_offset = C.get_offset
  let test = C.test
  let memop = C.memop
  let memopb = C.memopb
  let movzx = C.movzx
  let flagop = C.flagop
  let load_address = C.load_address
  let shift = C.shift
  let stackop = C.stackop
  let call = C.call
  let return = C.return
  let elapse = C.elapse
  let print form env = 
    Printf.printf "\n\n\n\n#######################\n\n";
    C.print form env
    
  let print_delta env1 form env2 = 
    Printf.printf "\n\n\n\n#######################\n\n";
    C.print_delta env1 form env2
    
  let read_instruction env addr = C.access_readonly env (Int64.add (Int64.of_int addr) !instruction_addr_base)

end


module NoInstructionCacheArchitectureAD (C: CALL_ABSTRACT_DOMAIN) : ARCHITECTURE_ABSTRACT_DOMAIN = struct
  include JointCacheArchitectureAD (C)

  let read_instruction env addr = env 
end


