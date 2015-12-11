module Intermediate2.Programs (Memory(..), evaluate, ababs, double, swap, memories) where

import Intermediate2.Memory (Memory(..))
import Intermediate2.Compile (compile)
import Intermediate2.StackMachine (evaluate, steps, initial, memories)

ababs :: Memory
ababs = compile "SII(S(K(ab))(SII))"

double :: Memory
double = compile "SIIa"

swap :: Memory
swap = compile "S(K(SI))Kab"

