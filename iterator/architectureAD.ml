open Signatures
open X86Types




(* Architecture abstract domain. Right now it allows two different caches for instructions and data *)

let instruction_addr_base = ref (Int64.of_int 0)



module type T =
  sig
    include Signatures.ABSTRACT_DOMAIN
    val init: X86Headers.t -> (X86Types.reg32 * int64 * int64) list -> cache_param -> cache_param option -> int64 -> t
    val get_offset: t -> op32 -> (int,t) finite_set
    val test : t -> X86Types.condition -> (t add_bottom)*(t add_bottom)
    val call : t -> op32 -> int -> (int,t) finite_set 
    val return : t -> (int,t) finite_set
    val memop : t -> memop -> op32 -> op32 -> t
    val memopb : t -> memop -> op8 -> op8 -> t
    val movzx : t -> op32 -> op8 -> t
    val load_address : t -> X86Types.reg32 -> X86Types.address -> t
    val flagop : t -> op32 flagop -> t
    val stackop : t -> stackop -> op32 -> t
    val shift : t -> X86Types.shift_op -> op32 -> op8 -> t
    val elapse : t -> int -> t
    val read_instruction: t -> int -> t
  end



module MakeSeparate (S: StackAD.T) (IC: CACHE_ABSTRACT_DOMAIN) = struct

  type t = {
    call_ad: S.t;
    inst_ad: IC.t
  }

  let init concr_mem start_values data_cache_params inst_cache_params addr_base = 
    instruction_addr_base := addr_base;
    {
      call_ad = S.init concr_mem start_values data_cache_params;
      inst_ad = IC.init (match inst_cache_params with
          Some(params) -> params
        | _ -> failwith "No/Invalid parameters supplied to instruction cache")
    }

  let subs_e env call_env = {env with call_ad=call_env}
  let subs_finite_set env fs = match fs with
    | Top(v) -> Top(subs_e env v)
    | Finite(l) -> Finite (List.map (fun (n,e) -> (n,subs_e env e)) l)

  
  let join env env2 = 
    let call_ad = S.join env.call_ad env2.call_ad in
    let inst_ad = IC.join env.inst_ad env2.inst_ad in
    {call_ad = call_ad; inst_ad = inst_ad}
  
  let widen env env2 =
    let call_ad = S.widen env.call_ad env2.call_ad in
    let inst_ad = IC.widen env.inst_ad env2.inst_ad in
    {call_ad = call_ad; inst_ad = inst_ad}

  let subseteq env env2 = S.subseteq env.call_ad env2.call_ad && IC.subseteq env.inst_ad env2.inst_ad

  let test env cond = 
    let subs_nb = function
    | Bot -> Bot
    | Nb(v) -> Nb(subs_e env v) in
    let (l,r) = (S.test env.call_ad cond) in
    (subs_nb l,subs_nb r)

  (* Redirect all usual stack calls to the stackAD *)
  let get_offset env op = subs_finite_set env (S.get_offset env.call_ad op)
  let memop env mop op1 op2 = subs_e env (S.memop env.call_ad mop op1 op2)
  let memopb  env mop op1 op2 = subs_e env (S.memopb env.call_ad mop op1 op2)
  let movzx env op1 op2 = subs_e env (S.movzx env.call_ad op1 op2)
  let flagop env fop = subs_e env (S.flagop env.call_ad fop)
  let load_address env reg add = subs_e env (S.load_address env.call_ad reg add)
  let shift env sop op1 op2 = subs_e env (S.shift env.call_ad sop op1 op2)
  let stackop env sop op1 = subs_e env (S.stackop env.call_ad sop op1) 
  let call env op n = subs_finite_set env (S.call env.call_ad op n)
  let return env = subs_finite_set env (S.return env.call_ad)
  let print form env = 
    Printf.printf "\n\n\n\n#######################\n\n\n------ Data Cache -----\n\n";
    S.print form env.call_ad;
    Printf.printf "\n\n\n-- Instruction Cache --\n";
    IC.print form env.inst_ad;
    Printf.printf "\n-----------------------\n\n"

  let print_delta env1 form env2 = 
    Printf.printf "\n\n\n\n#######################\n\n\n------ Data Cache -----\n\n";
    S.print_delta env1.call_ad form env2.call_ad;
    Printf.printf "\n\n\n-- Instruction Cache --\n\n";
    IC.print_delta env1.inst_ad form env2.inst_ad;
    Printf.printf "\n-----------------------\n\n"

  let elapse env t = {
    call_ad = S.elapse env.call_ad t;
    inst_ad = IC.elapse env.inst_ad t
  }

  let read_instruction env addr = { env with inst_ad = (IC.touch env.inst_ad (Int64.add (Int64.of_int addr) !instruction_addr_base)) }
  (* Instructions are isolated from data so it doesn't matter if the address is relative *)

end

module MakeShared (S: StackAD.T) = struct

  type t = S.t

  let init concr_mem start_values data_cache_params inst_cache_params addr_base =
    instruction_addr_base := addr_base;
    S.init concr_mem start_values data_cache_params

  (* Redirect all usual stack calls to the stackAD *)
  let join = S.join
  let widen = S.widen
  let subseteq = S.subseteq
  let get_offset = S.get_offset
  let test = S.test
  let memop = S.memop
  let memopb = S.memopb
  let movzx = S.movzx
  let flagop = S.flagop
  let load_address = S.load_address
  let shift = S.shift
  let stackop = S.stackop
  let call = S.call
  let return = S.return
  let elapse = S.elapse
  let print form env = 
    Printf.printf "\n\n\n\n#######################\n\n";
    S.print form env
    
  let print_delta env1 form env2 = 
    Printf.printf "\n\n\n\n#######################\n\n";
    S.print_delta env1 form env2
    
  let read_instruction env addr = S.access_readonly env (Int64.add (Int64.of_int addr) !instruction_addr_base)

end


module MakeDataOnly (S: StackAD.T) = struct
  include MakeShared (S)

  let read_instruction env addr = env 
end


