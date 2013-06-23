(** An [AgeAD]-implementation using relational sets for keeping track of ages *)

open NAD.DataStructures

module type S = AgeAD.S

module RelAgeAD : S
