open Signatures
module type T = sig
  include SimpleValAD.T
  val mem : t -> (var * int) list -> bool
  val partition: t -> var list list
end

module SimpleRelSetAD : T
