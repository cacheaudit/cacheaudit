open Signatures

module type T = sig
  include ABSTRACT_DOMAIN
  (* Initialize with a maximal value *)
  val init_with_max : (var->string) -> int -> t
  (* Increment variable, does not increase above the max value *)
  val inc_var : t -> var -> t
  (* Set variable to a value;
     if variable does not exist, create it *)
  val set_var : t -> var -> int -> t
  (* Filter domain according to simple comparison of two variables x1 and x2*)
  (* the first result approximates the cases when x1 < x2 and
     the second one when x1 > x2 *)
  val comp : t -> var -> var -> (t add_bottom)*(t add_bottom)
  (* Filter domain according to whether thvariable takes the value or not.
     the first result is the cases where the var age < max_value and the second one >= max_value *)
  val comp_with_val : t -> var -> int -> (t add_bottom)*(t add_bottom)
  (* returns the environments where the variable  take exactly that vallue *)
  val exact_val : t -> var -> int -> (t add_bottom)
  (* applies a permutation to the values of the variable *)
  val permute : t -> (int -> int) -> var -> t
  val get_values : t -> var -> int list
  val is_var : t -> var -> bool
end

module Make :
  functor (V : ValAD.T) -> T

