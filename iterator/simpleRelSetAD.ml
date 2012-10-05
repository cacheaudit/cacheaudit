open Printf
open Signatures
open AgeFunction
open AgeFunctionSet
open RelSetMap

let debug = ref false

module SimpleRelSetAD : SIMPLE_VALUE_AD  = struct
  module M = RelSetMap
  type t = {map : M.t; arity : int; max : int}

  let init_with_max v2s max = {map = M.init_with_max v2s max; arity = 2; max = max}

  let get_values (rsAD:t) (v:var) : int list = 
    let vset = VarSet.add v VarSet.empty in
    AFS.values (M.find vset rsAD.map) v

  let forget_rel_info map (v:var) = 
    M.filter (fun vset _ -> not (VarSet.mem v vset)) map

  exception NotImplemented
  let is_var env a = raise NotImplemented
  
  let set_var (rsAD:t) (v:var) (age:int) = 
    let map = forget_rel_info rsAD.map v in
    let vset = VarSet.add v VarSet.empty in
    {rsAD with map = M.add vset (AFS.singleton v age) map}

  let inc_var (rsAD:t) (v:var) =
    {rsAD with map = M.mapi (fun vset afs -> if VarSet.mem v vset then AFS.inc_var afs v rsAD.max else afs) rsAD.map}

  let add env v cv =
    failwith "simpleRelSetAD: add not implemented"
  
  let print (fmt:Format.formatter) (rsAD:t) = M.print fmt rsAD.map

  let print_delta (rsAD1:t) (fmt:Format.formatter) (rsAD2:t) = M.print_delta rsAD1.map fmt rsAD2.map 

  let join (rsAD1:t) (rsAD2:t) : t = 
    (* Determine all varSets which have different Age Function Sets. *)
    let diffs = M.differences rsAD1.map rsAD2.map in
    let new_map = List.fold_left (fun map (vset,afs1,afs2) -> 
      let joined_afs = AFS.join afs1 afs2 in
      M.add vset joined_afs map
      ) rsAD1.map diffs in
    (* Add relational Information where we can gain precision *)
    let new_map = List.fold_left 
      (
       fun map (vset,afs1,afs2) -> List.fold_left 
         (
          fun map' (vset',afs1',afs2') -> 
            let comb_vset = VarSet.union vset vset' in 
            if M.mem comb_vset map' || VarSet.cardinal comb_vset > rsAD1.arity then 
              map' 
            else
             let comb_afs1 = AFS.combine afs1 afs1' in
             let comb_afs2 = AFS.combine afs2 afs2' in
             M.add comb_vset (AFS.join comb_afs1 comb_afs2) map'
         ) map diffs
       ) new_map diffs in 
    {rsAD1 with map = new_map}

  let widen (rsAD1:t) (rsAD2:t) : t = join rsAD1 rsAD2

  let subseteq (rsAD1:t) (rsAD2:t) : bool = 
    M.for_all (fun vset afs1 -> AFS.subseteq afs1 (M.find vset rsAD2.map)) rsAD1.map

  (* Check if no Relation is empty, i.e. there is no valid position for some variable (combination) *)
  let check_validity rsAD = if M.for_all (fun k set -> not (AFS.is_empty set)) rsAD.map then Nb rsAD else Bot 

  let comp (rsAD:t) (v1:var) (v2:var) : (t add_bottom)*(t add_bottom) = 
    let v2_vals = AFS.values (M.find (VarSet.add v2 VarSet.empty) rsAD.map) v2 in 
    let sm_v2 = List.hd (List.sort Pervasives.compare v2_vals) in
    let gt_v2 = List.hd (List.sort (fun x y -> Pervasives.compare y x) v2_vals) in

    let update_ages (vset:VarSet.t) map (compare:int->int->int) (limit:int): AFS.t  = 
      let old_ages = M.find vset map in 
      match VarSet.mem v1 vset, VarSet.mem v2 vset with  
          true , true  -> AFS.filter (fun af -> compare (AF.get v1 af) (AF.get v2 af) = -1) old_ages (* compare v1 v2 = -1*)
        | true , false -> AFS.filter (fun af -> compare (AF.get v1 af) limit = -1) old_ages
        | false, true  -> old_ages
        | false, false -> old_ages in 
    let ls  = M.mapi (fun vset afs -> update_ages vset rsAD.map Pervasives.compare gt_v2) rsAD.map in
    let gt = M.mapi (fun vset afs -> update_ages vset rsAD.map (fun x y -> Pervasives.compare y x) sm_v2) rsAD.map in 
    (check_validity {rsAD with map = ls},check_validity {rsAD with map = gt})
end



(* 
   Speed comparison
           Linesize 1024  2048  512   256   128            32
   -----------------------------------------------------------------
   MapAgeFunction:  192s   98s  188s  763s 
   ListAgeFunction: 170s   99s  189s  741s 2519s->499s     8193s
*)
