module Intermediate.Programs (Memory(..), evaluate, ababs, double, swap, memories) where

import Intermediate.Memory (Memory(..))
import Intermediate.Compile (compile)
import Intermediate.StackMachine (evaluate, steps, initial, memories)

ababs :: Memory
ababs = compile "SII(S(K(ab))(SII))"

double :: Memory
double = compile "SIIa"

swap :: Memory
swap = compile "S(K(SI))Kab"

