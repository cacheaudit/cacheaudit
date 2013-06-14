open Signatures
module type S = sig
  include AgeAD.S
  val mem : t -> (var * int) list -> bool
  val partition: t -> var list list
end

module SimpleRelSetAD : S
