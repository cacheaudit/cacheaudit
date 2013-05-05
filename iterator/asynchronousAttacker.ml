open Signatures
open Big_int

let time_hit = 1
let time_miss = 10

let bits_big_int bn =
  log (float_of_big_int bn) /. (log 2.)

let min_frequency = ref 1 (* the minimum number of cycles between two peeks of the attacker *)

module IntMap = Map.Make(struct type t = int let compare = compare end)

(* we suppose an attecker that can choose the number of instructions elpased *)
(* This is only correct on programs without branching !!! ********************)
module InstructionBasedAttacker (C: CACHE_ABSTRACT_DOMAIN) : CACHE_ABSTRACT_DOMAIN = struct
  type one_attacker_age_state = {
    caches : C.t;
    leakage : big_int; (* we use big_int to be able to represent up to 2^258 configurations *)
  }


  let history_kept = ref 1

  type t = {
    history : one_attacker_age_state IntMap.t; (* where t.(i) is the state when the attacker didn't peek since at least i cycles. Exactly i cycles if i<max element *)
    init_cache : C.t; (* used to reset the cache state *)
  }

  let init cp = 
    if !history_kept == 1 then history_kept := !min_frequency;
    let initial_state = {
      caches = C.init cp;
      leakage = unit_big_int;
    } in
    let rec add_ages i s = if i > !history_kept then s
      else add_ages (i+1) (IntMap.add i initial_state s) in
    (* we don't make any assumption on the history before starting the program, since we could be at any round *)
    { history = add_ages 0 IntMap.empty;
      init_cache = initial_state.caches;
    }

  let touch env addr = { env with history = IntMap.fold 
   (fun i cs res -> IntMap.add i {cs with caches = C.touch cs.caches addr} res) 
   env.history IntMap.empty }

  (* safe, but very imprecise, TODO *)
  let touch_hm env addr = let t = touch env addr in Nb t, Nb t

  let leakage_of_attacker_state cs = 
    mult_big_int (C.count_cache_states cs.caches) cs.leakage
 
  (* Raises Not_found if no possible environment may interrupt *)
  let new_interrupt env =  
    if !min_frequency >= !history_kept then 
      leakage_of_attacker_state (IntMap.find !history_kept env)
    else let _, _, may_interrupt = IntMap.split (!min_frequency-1) env in
      if IntMap.is_empty may_interrupt then raise Not_found
      else IntMap.fold (fun _ cs m -> max_big_int m (leakage_of_attacker_state cs)) may_interrupt unit_big_int

  (* when we do elapse, we know we finished one instruction *)
  let elapse env _ = 
    let decale = IntMap.fold (fun i cs res -> IntMap.add (i+1) cs res) env.history
                          IntMap.empty in
    let cut_env = try (
      let to_merge1 = IntMap.find (!history_kept+1) decale 
      and to_merge2 = IntMap.find !history_kept decale in
      IntMap.remove (!history_kept+1) (IntMap.add !history_kept 
        {caches = C.join to_merge1.caches to_merge2.caches;
         leakage = max_big_int to_merge1.leakage to_merge2.leakage;
        } decale)
      ) with Not_found -> decale in
    {env with history = try IntMap.add 0 
         { caches = env.init_cache;
           leakage = (new_interrupt env.history) 
         }  cut_env with Not_found -> cut_env}

  let count_cache_states env = 
    IntMap.fold (fun _ cs sol -> add_big_int sol (C.count_cache_states cs.caches)) env.history zero_big_int
  
(* beware not to use this join on controlflow joins !!!*) 
  let join env1 env2 = (* Here we need a Map2...*)
    {env1 with history = IntMap.fold (fun k cs1 res -> 
                  let ncs = try 
                  let cs2 = IntMap.find k env2.history in
                  { caches = C.join cs1.caches cs2.caches;
                    leakage = max_big_int cs1.leakage cs2.leakage;
                  } 
                  with Not_found -> cs1 in
                  IntMap.add k ncs res) 
       env1.history env2.history }

  let widen env1 env2 = failwith "Widening not implemented\n"
  
  let subseteq env1 env2 = try 
    IntMap.for_all (fun k cs1 ->
                     let cs2 = IntMap.find k env2.history in
                     le_big_int cs1.leakage cs2.leakage
                     && C.subseteq cs1.caches cs2.caches
                   ) env1.history 
    with Not_found -> false

  let print_one_attacker_state fmt k cs = 
    Format.fprintf fmt "@[For age %d, leakage is at most %s, and cache may be %a@.@]" k (string_of_big_int cs.leakage) C.print cs.caches

  let print fmt env = 
    Format.fprintf fmt "@[";
    IntMap.iter (print_one_attacker_state fmt) env.history;
    Format.fprintf fmt "Maximal leakage is %f bits@.@]" (bits_big_int (IntMap.fold (fun _ cs m -> max_big_int m cs.leakage) env.history unit_big_int))
    
let print_delta env1 fmt env2 = () (*print fmt env2 (*TODO*) *)

end 

module type PRE_CACHE_ABSTRACT_DOMAIN = sig
  type cache
  val ctouch: cache -> int64 -> cache
  val ctouch_hm: cache -> int64 -> (cache add_bottom * cache add_bottom)
  val cjoin: cache -> cache -> cache
  type d = { observables : cache IntMap.t;
             in_progress : cache IntMap.t;
           }
  include ABSTRACT_DOMAIN with type t=d
  val init : cache_param -> t
  val count_cache_states : t -> Big_int.big_int
  val age_and_observe : t -> int -> t
  (* falta solo touch y elapse *)
end

module OneInterrupt(C:CACHE_ABSTRACT_DOMAIN):PRE_CACHE_ABSTRACT_DOMAIN = struct
  type cache = C.t
  let ctouch = C.touch
  let ctouch_hm = C.touch_hm
  let cjoin = C.join
  
  type d = { observables : C.t IntMap.t; (* will contain observables at each time step *)
             in_progress : C.t IntMap.t; (* partial traces to be completed *)
           }
  type t = d
  let map_join m1 m2 = IntMap.fold (fun k c1 res -> 
       let nc = try let c2 = IntMap.find k m2 in C.join c1 c2
                 with Not_found -> c1 
       in IntMap.add k nc res) m1 m2

  let init cp =
    let init_cache = C.init cp in
    { observables = IntMap.empty;
      in_progress = IntMap.singleton 0 init_cache;
    }

  let age_and_observe env d = 
    (* first we age in_progress *)
    let np = IntMap.fold (fun t c res -> IntMap.add (t+d) c res) 
                         env.in_progress IntMap.empty
    in
    (* then we add new observables *)
    let no = map_join np env.observables
    in {observables = no; in_progress = np} (* what about what we can observe during the duration of a command ? TODO *)

  let count_cache_states env = 
    IntMap.fold (fun _ cs sol -> add_big_int sol (C.count_cache_states cs)) env.in_progress zero_big_int
 
  let join env1 env2 =  
    { observables = map_join env1.observables env2.observables;
      in_progress = map_join env1.in_progress env2.in_progress;
    }

  let widen env1 env2 = failwith "Widening not implemented\n"

  let subseteq env1 env2 = 
     let map_subseteq m1 m2 = try IntMap.for_all (fun k c -> C.subseteq c (IntMap.find k m2)) m1 with Not_found -> false in
     map_subseteq env1.observables env2.observables && map_subseteq env1.in_progress env2.in_progress

  let print fmt env = 
    let max_leakage = IntMap.fold 
        (fun k c res -> max_big_int (C.count_cache_states c) res)
        env.observables unit_big_int

    in
    Format.fprintf fmt "@[Maximal leakage is %s,@, that is %f bits@.@]"
      (string_of_big_int max_leakage) (bits_big_int max_leakage)

  let print_delta env1 fmt env2 = IntMap.iter (fun t c -> Format.fprintf fmt "@[ At time %d, cache is currently %a@]" t C.print c) env1.in_progress

end

module OneInstructionInterrupt(C:CACHE_ABSTRACT_DOMAIN):CACHE_ABSTRACT_DOMAIN =
struct
  include OneInterrupt(C) 
  
  let touch env addr = {env with in_progress = IntMap.fold (fun t c res -> IntMap.add t (ctouch c addr) res) env.in_progress IntMap.empty}

  let touch_hm env addr = let t = touch env addr in Nb t, Nb t

  let elapse env d = age_and_observe env 1
end
    
module OneTimeInterrupt(C:CACHE_ABSTRACT_DOMAIN):CACHE_ABSTRACT_DOMAIN =
struct
  include OneInterrupt(C)

  let add_at c t acc = match c with
    Bot -> acc
  | Nb cache -> 
      let nc = try cjoin cache (IntMap.find t acc)
               with Not_found -> cache
      in
      IntMap.add t nc acc

  let touch env addr = { env with in_progress =
    IntMap.fold (fun t c res ->
          let c_hit,c_miss = ctouch_hm c addr in
          add_at c_hit (t+time_hit) (add_at c_miss (t+time_miss) res)
                ) env.in_progress IntMap.empty
                       }

  let touch_hm env d = failwith "touch_hm not implemented in OneTimeInterrupt"

  let elapse env d = age_and_observe env d
end
