module type VALADOPT =
  sig val max_get_var_size : int val max_set_size : int end
module ValADOptForMemory :
  sig val max_get_var_size : int val max_set_size : int end

module ValADFunctor :
  functor (O : VALADOPT) -> Signatures.VALUE_ABSTRACT_DOMAIN
