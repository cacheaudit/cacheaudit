open Signatures
open AgeFunction

module AF = PairListAgeFunction

module type AGE_FUNCTION_SET = sig
  type t 
  val combine : t -> t -> t (* Combines two AgeFunctionSets with distinct variables *)
  (* True if there is a common variable v , s.t. there is no AF' in AFS with AF(v)=AF'(v) *)  
  val contradicts: t -> AF.t -> bool 
  val empty : t
  val equal : t -> t -> bool
  val filter : (AF.t -> bool) -> t -> t
  val inc_var : t -> var -> int -> t
  val is_empty : t -> bool
  val join : t -> t -> t   (* Joins two AgeFunctionSets with the same variables *)
  val singleton : var -> int -> t
  val subseteq : t -> t -> bool
  val toString : t -> string
  val values : t -> var -> int list
  val vset : t -> VarSet.t
  
end

module AgeFunctionSet : AGE_FUNCTION_SET = struct
  module S = Set.Make(struct type t = AF.t let compare = AF.compare end)
  type t = {set : S.t; vars : VarSet.t}

  let is_empty afs : bool = S.is_empty afs.set

  let combine afs1 afs2 = 
    if is_empty afs1 then afs2 else
    if is_empty afs2 then afs1 else
    let cross_join set1 set2 = S.fold (fun af1 set -> S.fold (fun af2 set' -> S.add (AF.join af1 af2) set') set2 set) set1 S.empty in
    {set = cross_join afs1.set afs2.set; vars = VarSet.union afs1.vars afs2.vars}

  let project afs vlist = 
    {set = S.fold (fun e set'-> S.add (AF.project e vlist) set') afs.set S.empty; 
     vars = VarSet.filter (fun v -> List.mem v vlist) afs.vars}

  let contradicts (afs:t) (af:AF.t) = 
    let common_vars : VarSet.t = VarSet.inter afs.vars (AF.vars af) in
    not (S.exists (fun (af':AF.t) -> VarSet.for_all (fun (v:var) -> (AF.get v af) = (AF.get v af')) common_vars) afs.set)
    

  let empty = {set = S.empty; vars = VarSet.empty}

  let equal afs1 afs2 : bool = 
    if VarSet.equal afs1.vars afs2.vars then
      S.equal afs1.set afs2.set
    else false

  let filter f afs : t = {afs with set = S.filter f afs.set}

  let inc_var afs v max = 
    {afs with set = S.fold (fun af set -> S.add (AF.add v (Pervasives.min (AF.get v af + 1) max) af) set) afs.set S.empty}

  let join afs1 afs2 = {afs1 with set = S.union afs1.set afs2.set}

  let singleton v i = {set = S.add (AF.add v i AF.empty) S.empty; vars = VarSet.add v VarSet.empty}

  let subseteq afs1 afs2 : bool = S.subset afs1.set afs2.set

  let toString afs : string =
    let s = S.fold (fun e s -> s ^ (AF.toString e) ^ ";") afs.set "" in
    if String.length s = 0 then "{ }" else "{" ^ (String.sub s 0 (String.length s -1)) ^ "}" 

  let values afs v = S.fold (fun af l -> let value = AF.get v af in if List.mem value l then l else value::l) afs.set []

  let vset afs = afs.vars

end

