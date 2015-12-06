module Intermediate.Programs (evaluate, ababs, double, swap) where

import Intermediate.Memory (Memory)
import Intermediate.Compile (compile)
import Intermediate.StackMachine (evaluate, steps, initial)

ababs :: Memory
ababs = compile "SII(S(K(ab))(SII))"

double :: Memory
double = compile "SIIa"

swap :: Memory
swap = compile "S(K(SI))Kab"

