module Intermediate3.Programs (Memory(..), evaluate, ababs, double, swap, memories) where

import Intermediate3.Memory (Memory(..))
import Intermediate3.Compile (compile)
import Intermediate3.StackMachine (evaluate, steps, memories)

ababs :: Memory
ababs = compile "SII(S(K(ab))(SII))"

double :: Memory
double = compile "SIIa"

swap :: Memory
swap = compile "S(K(SI))Kab"

