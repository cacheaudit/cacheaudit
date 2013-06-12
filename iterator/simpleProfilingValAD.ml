open Signatures

module SM = Map.Make(struct type t = string let compare = Pervasives.compare end)

let calls : int SM.t ref = ref SM.empty
let cTime : float SM.t ref = ref SM.empty
let sumTime : float SM.t ref = ref SM.empty

module Make(S:AgeAD.T) = struct
  type t = S.t

  let start (fn : string) : unit = 
      calls := if SM.mem fn !calls then SM.add fn (SM.find fn !calls + 1) !calls else SM.add fn 1 !calls;
      cTime := SM.add fn (Sys.time ()) !cTime

  let stop (fn : string) : unit = 
      let diff = Sys.time () -. SM.find fn !cTime in
      sumTime := if SM.mem fn !sumTime then SM.add fn (SM.find fn !sumTime +. diff) !sumTime else SM.add fn diff !sumTime

  let comp ad v1 v2 = 
    start "comp";
    let r = S.comp ad v1 v2 in
    stop "comp";
    r

  let comp_with_val ad v c =
    start "comp_with_val";
    let r = S.comp_with_val ad v c in
    stop "comp_with_val";
    r

  let exact_val ad v c = 
    start "exact_val";
    let r = S.exact_val ad v c in
    stop "exact_val";
    r


  let permute ad f v = 
    start "permute";
    let r = S.permute ad f v in
    stop "permute";
    r




  exception NotImplemented
  let is_var env a = raise NotImplemented
  
  let get_values ad v = 
    start "get_values";
    let r = S.get_values ad v in
    stop "get_values";
    r

  let inc_var ad v = 
    start "inc_var";
    let r = S.inc_var ad v in
    stop "inc_var";
    r
  
  let init_with_max f max = 
    S.init_with_max f max 

  let join ad1 ad2 = 
    start "join";
    let r = S.join ad1 ad2 in
    stop "join";
    r

  let print_map fmt title map toString = 
    let s = SM.fold (fun fn v s -> s ^ fn ^ ": " ^ (toString v) ^ "\n") map "" in
    Format.fprintf fmt "%s: \n%s\n" title s

  let print fmt ad  = 
    S.print fmt ad;
    print_map fmt "\nFunction Calls" !calls (string_of_int);
    print_map fmt "Summarized Execution Time" !sumTime (fun x -> string_of_int (int_of_float x) ^ "s")
    (*;let avgTime = SM.fold (fun fn time map -> SM.add fn (time /. (float_of_int (SM.find fn !calls))) map) !sumTime SM.empty in
    print_map "Average Execution Time" avgTime string_of_float*)

  let print_delta ad1 fmt ad2 = 
    start "print_delta";
    S.print_delta ad1 fmt ad2;
    stop "print_delta"

  let set_var ad v i = 
    start "set_var";
    let r = S.set_var ad v i in
    stop "set_var";
    r
  
  let subseteq ad1 ad2 = 
    start "subseteq";
    let r = S.subseteq ad1 ad2 in
    stop "subseteq";
    r

  let widen ad1 ad2 = 
    start "widen";
    let r = S.widen ad1 ad2 in
    stop "widen";
    r
end

