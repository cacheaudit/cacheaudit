val min_frequency : int ref

module InstructionBasedAttacker :
  functor (C : CacheAD.T) -> CacheAD.T
module OneInstructionInterrupt :
  functor (C : CacheAD.T) -> CacheAD.T
module OneTimeInterrupt :
  functor (C : CacheAD.T) -> CacheAD.T
    
