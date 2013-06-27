(** An [AgeAD]-implementation using relational sets for keeping track of ages *)

open NumAD.DS

module type S = AgeAD.S

module RelAgeAD : S
