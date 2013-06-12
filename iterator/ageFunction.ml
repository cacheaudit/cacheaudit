open Signatures

module VarSet = Set.Make(struct type t = var let compare = Pervasives.compare end)

module AgeMap = Map.Make(struct type t = var let compare = Pervasives.compare end)

(* An AgeFunction module provides a mapping from variables to their respective age. *)
module type AGE_FUNCTION = sig
  type t

  (* <add v i af> adds a mapping from variable v to the value i to the AgeFunction af. *)
  val add :var -> int -> t -> t
  (* Compares two AgeFunctions. The result should be zero iff they have the same input space and the bound value is equal in both maps for all values. *)
  val compare : t -> t -> int 
  (* Returns a mapping in which no variable is bound. *)
  val empty : t
  (* Returns the most recent binding for the variable. *)
  val get : var -> t -> int
  (* <project af l> Returns an age function only consisting of variables in l. *)
  (* l is expected to contain only variables in used in af.vars *)
  val project : t -> var list -> t
  (* Given two maps with distinct input spaces, it returns a mapping that contains the mapping of both AgeFunctions. *)
  val join : t -> t -> t
  (* Returns a string representation of the AgeFunction. *)
  val toString : t -> string
  (* Returns the set of variables for which there exists a mapping. *)
  val vars : t -> VarSet.t
end

module AgeFunction : AGE_FUNCTION = struct
  type t = (var * int) list
  
  let rec compare af1 af2 : int = match af1,af2 with
     [],[] -> 0
   | (_,i1)::tl1,(_,i2)::tl2 -> let c = Pervasives.compare i1 i2 in if c = 0 then compare tl1 tl2 else c
   |  _,_  -> -1

  let empty = []

  let rec get v af = 
    match af with
      (v',i)::tl  -> if v = v' then i else get v tl
    |    _        -> failwith "Unexpected case in AgeFunction.get"

  let rec add v i af = 
    match af with 
         []       -> [(v,i)]
    | (v',i')::tl -> match Pervasives.compare v v' with
                     -1 -> (v,i)::af
                  |   0 -> (v,i)::tl
                  |   1 -> (v',i')::add v i tl
                  |   _ -> failwith "Unexpected case in AgeFunction.add"

  let rec join af1 af2 = 
    match af1,af2 with
  | [],l | l,[] -> l
  | (v1,i1)::t1,(v2,i2)::t2 -> let cmp = Pervasives.compare v1 v2 in
    if  cmp < 0 then (v1,i1)::(join t1 af2)
    else if cmp > 0 then (v2,i2)::(join af1 t2)
    else failwith "Unexpected case in AgeFunction.join"

  let toString af = 
    let s = List.fold_left (fun s (_,e) -> s ^ (string_of_int e) ^ ",") "" af in
    "(" ^ (String.sub s 0 (String.length s -1)) ^ ")" 
     
  let rec project af l = 
    match af,l with 
    |  (v1,i)::tl1,v2::tl2 -> let cmp = Pervasives.compare v1 v2 in
       if cmp = 0 then (v1,i)::project tl1 tl2
       else if cmp < 0 then project tl1 l
       else failwith "Unexpected case in AgeFunction.project"
    |    _, [] -> []
    |    _,_ -> failwith "Unexpected case in AgeFunction.project"

  let rec vars af = match af with
     [] -> VarSet.empty
   | (v,i)::l -> VarSet.add v (vars l)
 end

