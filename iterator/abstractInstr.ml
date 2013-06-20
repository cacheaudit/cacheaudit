(** module defining types for the abstract instructions used by the iterator to communicate with teh abstract domains *)

type 'a flagop = ADcmp of 'a*'a
            | ADtest of 'a*'a 
            | ADfset of X86Types.flag*bool

type memop = ADarith of X86Types.arith_op | ADmov | ADexchg

type stackop = ADpop | ADpush

