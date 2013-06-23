(** Value abstract domain: overapproximates possible values of variables *)

module type VALADOPT = sig 
  val max_get_var_size : int 
  val max_set_size : int 
end

module ValADOptForMemory : sig 
  val max_get_var_size : int 
  val max_set_size : int 
end

module Make : functor (O : VALADOPT) -> NAD.S
