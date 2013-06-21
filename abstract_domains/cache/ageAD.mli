(** Abstract domain that keeps track of the "ages" of variables 
representing lines of memory

 - it knows tha maximal age [max_age] of the variable
 - it knows that the variables names are partitioned into sets
 - it knows what a valid cache configuration is 
(e.g. variables from one set cannot have the same age, unless it is [max_age]
 - it can count the number of valid cache configurations *)

open Big_int
open AD.DataStructures
open NAD.DataStructures

(** Output signature of [AgeAD.Make] *)
module type S = sig
  include AD.S
  val init : int -> (var -> int) -> (var->string) -> t
  (** [init maxval pfn v2s] Initialize with a maximal value [maxval]. 
       [pfn] is a partitioning function of the variables according to their 
       name and is used for counting 
       [v2s] is a function which converts variable names to strings *)
  val inc_var : t -> var -> t
  (** Increment variable, does not increase above the max value *)
  val set_var : t -> var -> int -> t
  (** Set variable to a value;
     if variable does not exist, create it *)
  val comp : t -> var -> var -> (t add_bottom)*(t add_bottom)
  (** Filter domain according to simple comparison of two variables [x1] and [x2]
     the first result approximates the cases when [x1] < [x2] and
     the second one when [x1] > [x2] *)
  val comp_with_val : t -> var -> int -> (t add_bottom)*(t add_bottom)
  (** Filter domain according to whether thvariable takes the value or not.
     the first result is the cases where the var [age] < [max_value] and the second one >= max_value *)
  val exact_val : t -> var -> int -> (t add_bottom)
  (** returns the environments where the variable  take exactly that value *)
  val permute : t -> (int -> int) -> var -> t
  (** applies a permutation to the values of the variable *)
  val get_values : t -> var -> int list
  (* val is_var : t -> var -> bool *)
  val delete_var : t -> var -> t
  val count_cstates: t -> big_int * big_int
end

(** Functor creating the age abstract domain given a value abstract domain *)
module Make :
  functor (V : NAD.S) -> S

