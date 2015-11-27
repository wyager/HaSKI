module Intermediate.Programs where

import Intermediate.Memory (Memory)
import Intermediate.Parser (compile)
import Intermediate.StackMachine (evaluate)

ababs :: Memory
ababs = compile "SII(S(K(ab))(SII))"

double :: Memory
double = compile "SIIa"

swap :: Memory
swap = compile "S(K(SI))Kab"