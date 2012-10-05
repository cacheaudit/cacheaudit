open Signatures

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
  (* Given two maps with distinct input spaces, it returns a mapping that contains the mapping of both AgeFunctions. *)
  val join : t -> t -> t
  (* Returns a string representation of the AgeFunction. *)
  val toString : t -> string
end

module VarSet = Set.Make(struct type t = var let compare = Pervasives.compare end)

module AgeMap = Map.Make(struct type t = var let compare = Pervasives.compare end)

(* An implementation of an AgeFunction using Maps *)
module MapAgeFunction : AGE_FUNCTION = struct
  type t = int AgeMap.t

  let add v i af = AgeMap.add v i af
  let compare af1 af2 = Pervasives.compare af1 af2(*AgeMap.compare (fun age1 age2 -> Pervasives.compare age1 age2) af1 af2*)
      
  let get v af = AgeMap.find v af

  let join af1 af2 = AgeMap.merge (fun v opt1 opt2 -> match opt1,opt2 with 
              Some x,None -> Some x | None, Some x -> Some x | _,_ -> failwith "AF.join: unexpected case") af1 af2
  let empty = AgeMap.empty
  let toString af = 
    let s = AgeMap.fold (fun v e s -> s ^ (string_of_int e) ^ ",") af "" in
    "(" ^ (String.sub s 0 (String.length s -1)) ^ ")" 
end

(* An implementation of an AgeFunction using Lists *)
module ListAgeFunction : AGE_FUNCTION = struct
  type t = {vals : int list; vars : VarSet.t}
  
  let compare af1 af2 : int = Pervasives.compare af1.vals af2.vals
    (*let c = Pervasives.compare af1.vars af2.vars in(*VarSet.compare af1.vars af2.vars in*)
    if c = 0 then 
      Pervasives.compare af1.vals af2.vals
    else 
      c*)

  (* insert element e at position n in the list l *)
  let rec list_insert (n:int) (l: 'a list) (e: 'a) : 'a list = 
    match n with 0 -> e :: l | m -> List.hd l :: list_insert (n-1) (List.tl l) e

  (* replace the element at position n in the list l with element e *)
  let rec list_replace (n:int) (l: 'a list) (e: 'a) : 'a list = 
    match n with 0 -> e :: (List.tl l) | m -> List.hd l :: list_replace (n-1) (List.tl l) e

  (* Computes the position of v in the set or the position where it would get inserted (if it is no element of set) *)
  let pos set v = 
    match VarSet.split v set with (l,e,g) -> VarSet.cardinal l

  let get v af = let pos_v = pos af.vars v in
    List.nth af.vals pos_v

  let add v i af = 
    if VarSet.mem v af.vars then
      {af with vals = list_replace (pos af.vars v) af.vals i}
    else
      {vals = list_insert (pos af.vars v) af.vals i; vars = VarSet.add v af.vars}

  let join af1 af2 = VarSet.fold (fun v af' -> add v (get v af2) af') af2.vars af1

  let toString af = 
    let s = List.fold_left (fun s e -> s ^ (string_of_int e) ^ ",") "" af.vals in
    "(" ^ (String.sub s 0 (String.length s -1)) ^ ")" 
 
  let empty = {vals = []; vars = VarSet.empty}
end


module PairListAgeFunction : AGE_FUNCTION = struct
  type t = (var * int) list
  
  let rec compare af1 af2 : int = match af1,af2 with
     [],[] -> 0
   | (_,i1)::tl1,(_,i2)::tl2 -> let c = Pervasives.compare i1 i2 in if c = 0 then compare tl1 tl2 else c
   |  _,_  -> -1

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

  let join af1 af2 = 
    List.fold_left (fun l (v,i) -> add v i l) af1 af2
     

  let toString af = 
    let s = List.fold_left (fun s (_,e) -> s ^ (string_of_int e) ^ ",") "" af in
    "(" ^ (String.sub s 0 (String.length s -1)) ^ ")" 
 
  let empty = []
end

