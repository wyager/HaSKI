module Hardware.Programs (Memory(..), evaluate, compile, ababs, double, swap) where

import CLaSH.Prelude

import Hardware.Memory (Memory(..))
import Hardware.Compile (compile)
import Hardware.Harness (evaluate)

ababs :: Memory
ababs = compile "SII(S(K(ab))(SII))"

double :: Memory
double = compile "SIIa"

swap :: Memory
swap = compile "S(K(SI))Kab"

