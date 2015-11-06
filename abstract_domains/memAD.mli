(* Copyright (c) 2013-2015, IMDEA Software Institute.          *)
(* See ../LICENSE for authorship and licensing information     *)

open X86Types
open AbstrInstr
open AD.DS
open Config

(** Memory abstract domain: maps machine operations to operations on integer variables *)

module type S =
sig
  include AD.S
    
 (** Creates a MemAD with given parameters. More specifically, in 
      [init iv mem dcp]     
       - [iv] represents initial values of memory locations
       - [mem] are initial values of registers
       - [dcp] is the configuration of the data cache, and
  *)
  val init: (int64 -> int64 option) -> Config.mem_param -> CacheAD.cache_param_t -> t
    
 (** [get_vals env op] returns a finite set of possible values for an op32 
      operand (which is a register/memory address/immediate), and the 
      respective environments.  In case no finite set can be determied, 
      returns Top. *)
  val get_vals: t -> op32 -> (int,t) finite_set

  (** Returns an overapproximation of the environments in which the condition holds,
      followed by an overapproximation of the environments in which it doesn't. *)
  val test : t -> condition -> (t add_bottom)*(t add_bottom)
  
  (** Interpret an instruction, passing its effects to CacheAD and FlagAD *)
  val interpret_instruction : t -> X86Types.instr -> t
    
  (** Signals to the cache that a memory location has been accessed *)  
  val touch : t -> int64 -> NumAD.DS.rw_t -> t

  val set_value: t -> int64 -> int64 -> t


  (** Signals from the iterator to the cache the time
      consumed by an instruction *)
  val elapse : t -> int -> t
end
    
(** Creates a memory abstract domain from a flag and cache abstract domains *)
module Make :
  functor (F : FlagAD.S) ->
    functor (C : CacheAD.S) -> S

