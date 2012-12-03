open Signatures
open AgeFunction
open AgeFunctionSet

module AFS = AgeFunctionSet

module VarSetMap = Map.Make(struct type t = VarSet.t let compare x y = let c = Pervasives.compare (VarSet.cardinal x) (VarSet.cardinal y) in if c = 0 then VarSet.compare x y else c end)

module type REL_SET_MAP = sig
  type t

  val init_with_max : (var -> string) -> int -> t
  val keys : t -> VarSet.t list
  val find : VarSet.t -> t -> AFS.t
  val add: VarSet.t -> AFS.t -> t -> t
  val filter : (VarSet.t -> AFS.t -> bool) -> t -> t
  val mapi : (VarSet.t -> AFS.t -> AFS.t) -> t -> t
  val print : Format.formatter -> t -> unit
  val print_delta : t -> Format.formatter -> t -> unit
  val differences : t -> t -> (VarSet.t * AFS.t * AFS.t) list
  val mem : VarSet.t -> t -> bool
  val for_all : (VarSet.t -> AFS.t -> bool) -> t -> bool
end

module RelSetMap : REL_SET_MAP = struct
  module M = VarSetMap
  type map = AFS.t M.t
  type t = {max : int; map : map; v2s : var -> string}

  let init_with_max v2s max = 
    {max = max;map = M.empty; v2s=v2s}

  let vset_to_string vset v2s  = let s = VarSet.fold (fun e s -> s ^ (v2s e) ^ ",") vset "" in
     "(" ^ (String.sub s 0 (String.length s -1)) ^ ")" 

  (* Returns the set of keys that are in the map. *)
  let keys rsMap : VarSet.t list = 
    M.fold (fun vset _ l -> vset::l) rsMap.map []

  let outside (v: var) (max:int) : AFS.t = AFS.singleton v max

  (* Returns a list of all subrelations contained in this relation. *)
  let subrelations (vset:VarSet.t) : (VarSet.t * var) list = VarSet.fold (fun v l -> (VarSet.remove v vset,v)::l) vset []

  (* Given a VarSet of size n, returns all subsets of size n-1. *)
  let subsets vset = VarSet.fold (fun v l -> VarSet.remove v vset::l) vset [] 

  (* Returns the AgeFunctionSet for the given VarSet. 
     If it is not contained in the map, it is created by considering its respective subsets. *)
  let rec find (vset:VarSet.t) (rsMap:t) : AFS.t = 
   let map = rsMap.map in 
   if (M.mem vset map) then
      M.find vset map
    else
      let all_subs = subrelations vset in 
      let subs_in_map = List.filter (fun (vset',v) -> M.mem vset' map) all_subs in
      match VarSet.cardinal vset with
      (*TODO Generalize*)
        1 -> outside (VarSet.choose vset) rsMap.max
      | 2 -> (
              match List.nth all_subs 0,List.nth all_subs 1 with 
              ((sub1,_),(sub2,_)) -> AFS.combine (find sub1 rsMap) (find sub2 rsMap)
             )
     | 3 -> if List.length subs_in_map >= 1 then 
             (
               match List.nth subs_in_map 0 with 
               (sub,v) -> AFS.combine (find sub rsMap) (find (VarSet.add v VarSet.empty) rsMap) 
             )
            else
              (
               match List.nth all_subs 0 with 
              (sub,v) -> AFS.combine (find sub rsMap) (find (VarSet.add v VarSet.empty) rsMap) 
             )
     | _ -> failwith ("get_afs: case not implemented yet" ^ (vset_to_string vset Int64.to_string))


  let is_redundant vset afs rsMap = 
    if VarSet.cardinal vset = 2 then 
      let afs' = List.fold_left (fun afs' vset' -> AFS.combine afs' (find vset' rsMap)) AFS.empty (subsets vset) in
      AFS.equal afs' afs
    else
      false  (* TODO outside -> redundant *)

  let add vset afs rsMap = 
    if is_redundant vset afs rsMap then 
      try {rsMap with map = M.remove vset rsMap.map} with Not_found -> rsMap
    else 
        {rsMap with map = M.add vset afs rsMap.map}

  let filter f rsMap = {rsMap with map = M.filter f rsMap.map}

  let mapi f rsMap = {rsMap with map = M.mapi f rsMap.map}

  let mem vset rsMap = M.mem vset rsMap.map

  let for_all f rsMap = M.for_all f rsMap.map

  (* Returns all VarSets and the two corresponding AgeFunctionSets in which the maps differ. 
     join_keysets should be called before.
  *)
  let differences rsMap1 rsMap2 : (VarSet.t * AFS.t * AFS.t) list = 
    let compare x y = let c = Pervasives.compare (VarSet.cardinal x) (VarSet.cardinal y) in if c = 0 then VarSet.compare x y else c in
    let rec diffs bds1 bds2 = 
      match bds1,bds2 with
      | (vset1,afs1)::tl1,(vset2,afs2)::tl2 -> (match compare vset1 vset2 with
                                              | -1 -> (vset1,afs1,find vset1 rsMap2)::diffs tl1 bds2
                                              |  0 -> if AFS.equal afs1 afs2 then diffs tl1 tl2 else (vset1,afs1,afs2)::diffs tl1 tl2
                                              |  1 -> (vset2,find vset2 rsMap1,afs2)::diffs bds1 tl2
                                              |  _ -> failwith "Unexpected case")
      | (vset1,afs1)::tl1,[]                -> (vset1,afs1,find vset1 rsMap2)::diffs tl1 bds2
      |                [],(vset2,afs2)::tl2 -> (vset2,find vset2 rsMap1,afs2)::diffs bds1 tl2
      |                [],[]                -> [] in

    diffs (M.bindings rsMap1.map) (M.bindings rsMap2.map)

  let print (fmt:Format.formatter) (rsMap:t) = 
     let b = Buffer.create (16) in 
     M.iter (fun vset afs -> Buffer.add_string b (vset_to_string vset rsMap.v2s); Buffer.add_string b " in "; Buffer.add_string b (AFS.toString afs);Buffer.add_string b "\n") rsMap.map;
     Format.fprintf fmt "%s" (Buffer.contents b)

  let print_delta (rsMap1:t) (fmt:Format.formatter) (rsMap2:t) = 
    let b = Buffer.create (16) in 
    List.iter (fun (vset,afs1,afs2) -> 
       Buffer.add_string b (vset_to_string vset rsMap2.v2s); Buffer.add_string b " in "; Buffer.add_string b (AFS.toString afs2);Buffer.add_string b "\n"
      ) (differences rsMap1 rsMap2);
    Format.fprintf fmt "%s" (Buffer.contents b)
end


