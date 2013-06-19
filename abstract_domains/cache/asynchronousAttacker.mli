val min_frequency : int ref

module InstructionBasedAttacker :
  functor (C : CacheAD.S) -> CacheAD.S
module OneInstructionInterrupt :
  functor (C : CacheAD.S) -> CacheAD.S
module OneTimeInterrupt :
  functor (C : CacheAD.S) -> CacheAD.S
    
