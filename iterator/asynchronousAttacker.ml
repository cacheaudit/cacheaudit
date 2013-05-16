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

module OneInstructionInterrupt(C:CACHE_ABSTRACT_DOMAIN):CACHE_ABSTRACT_DOMAIN =
struct
  (*Simple implementation where we assume that there is no branching, so that we don't need to store cache states at each position in time *)

  type t = { cache : C.t;
             leakage : big_int; (* maximun of the size of previous caches states *)
           }

  let init cp = { cache = C.init cp; leakage = unit_big_int }

  let touch x addr = { x with cache = C.touch x.cache addr }

  let touch_hm x addr = failwith "touch_hm not implemented in OneInstructionInterrupt"

  let elapse x _ = {x with leakage = max_big_int x.leakage (C.count_cache_states x.cache)}

  let count_cache_states x = failwith "count_cache_states not implemented in OneInstructionInterrupt"

  let join x1 x2 = assert(x1.leakage == x2.leakage);
   { x1 with cache = C.join x1.cache x2.cache}
                   
  (* for the widening, we just widen the caches and take the max of leakages, which must be the second argument *)
  (* This terminates because there is a bound on the number of cache states, but we could use thresholds... *) 
  let widen x1 x2 = { x2 with cache = C.widen x1.cache x2.cache }

  let subseteq x1 x2 = le_big_int x1.leakage x2.leakage && (C.subseteq x1.cache x2.cache)

  let print_leakage fmt x = Format.fprintf fmt "@[Maximal leakage is %s (that is %f bits)@]" (string_of_big_int x.leakage) (bits_big_int x.leakage)

  let print fmt x = Format.fprintf fmt "@[%a@.%a@]" C.print x.cache print_leakage x

  let print_delta x1 fmt x2 = Format.fprintf fmt "@[%a@,%a@]" (C.print_delta x1.cache) x2.cache print_leakage x2

end

(* This is not correct, as it does not take into account meddling of the attacker in the cache hits and misses *)    
module OneTimeInterrupt(C:CACHE_ABSTRACT_DOMAIN):CACHE_ABSTRACT_DOMAIN =
struct
  
  type t = { observables : C.t IntMap.t; (* will contain observables at each time step *)
             in_progress : C.t IntMap.t; (* partial traces to be completed *)
           }
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


  let add_at c t acc = match c with
    Bot -> acc
  | Nb cache -> 
      let nc = try C.join cache (IntMap.find t acc)
               with Not_found -> cache
      in
      IntMap.add t nc acc

  let touch env addr = { env with in_progress =
    IntMap.fold (fun t c res ->
          let c_hit,c_miss = C.touch_hm c addr in
          add_at c_hit (t+time_hit) (add_at c_miss (t+time_miss) res)
                ) env.in_progress IntMap.empty
                       }

  let touch_hm env d = failwith "touch_hm not implemented in OneTimeInterrupt"

  let elapse env d = age_and_observe env d
end
