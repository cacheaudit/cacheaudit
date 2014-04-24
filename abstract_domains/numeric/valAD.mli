(** Value abstract domain: overapproximates possible values of
    variables by sets and intervals *)

open NumAD.DS
open AD.DS

(** Module type for describing options of the value abstract domain *)
module type VALADOPT = sig 
  (** Threshold for returning dedicated environment with each
      variable value *)
  val max_get_var_size : int 

  (** Threshold for switching from explicit set representation to
      intervals (and back) *)
  val max_set_size : int 
end

(** Options for saving memory during analysis *)
module ValADOptForMemory : sig 
  val max_get_var_size : int 
  val max_set_size : int 
end

module type S = sig 
  include AD.S

  (** Initializes an empty numeric AD by specifying how variable names
      are printed *)
  val init : (var->string) -> t

  (** Create new variable *)
  val new_var : t -> var -> t

  (** Delete variable *)
  val delete_var : t -> var -> t

  (** Log the current values of a variable to the log file. For automated testing *)
  val log_var : var -> t -> unit

  (** [get_var env var] returns the current values of a variable
      [var], in form of a Map from values of [var] to environments in
      which [var] has that value. This interface can be used to
      capture relational information between variables (but could be
      substituted by a simpler/more efficient one for non-relational
      domains) *)
  val get_var : t -> var -> (t NumMap.t) add_top

  (** [set_var env var l h] sets the value of [var] to be in the
      interval [l,h] *)
  val set_var : t -> var -> int64 -> int64 -> t

 (** Checks if a variable is represented by the domain *)
  val is_var : t -> var -> bool

  (** Returns the variables represented by the domain *)
  val var_names : t -> NumSet.t

  (** Meet operation *)
  val meet : t -> t -> t add_bottom 

  (** [update_val env flags dst mskdst src msksrc op arg3] performs operation
      [op] on [dst] and [src], where the masks [mskdst] and [msksrc]
      specify whether 8 or 32 bit of the operand are involved. 
      [flags] gives the initial value of the flags. 
      [arg3] is an optional argument currently only used for 3-argument IMUL.
      Returns
      one environment per value combination of CF and ZF. *)
  val update_val : t -> flags_t -> var -> mask -> cons_var -> mask -> 
    AbstrInstr.abstr_op -> int64 option -> t FlagMap.t 
  (* This interface should be changed to allow flags as argument and
      return a tree *)
  (** [updval_set env flags dst mask op] performs a Set-instruction *)
  val updval_set : t -> flags_t -> var -> mask -> X86Types.cc -> t FlagMap.t
end



(** Creates a numeric abstract domain with the given options *)
module Make : functor (O : VALADOPT) -> S
