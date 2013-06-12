open Signatures
module type T = sig
  include AgeAD.T
  val mem : t -> (var * int) list -> bool
  val partition: t -> var list list
end

module SimpleRelSetAD : T
