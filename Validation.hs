module Validation where

import Intermediate.Programs as P1
import Intermediate2.Programs as P2

import Intermediate.Model as M1
import Intermediate2.Model as M2

convert' :: M1.SKI -> M2.SKI
convert' M1.S = M2.S
convert' M1.K = M2.K
convert' M1.I = M2.I
convert' (M1.T (M1.Ptr a) (M1.Ptr b)) = M2.T (M2.Ptr a) (M2.Ptr b)
convert' (M1.L c) = M2.L c

convert :: P1.Memory -> P2.Memory
convert (P1.Memory map) = P2.Memory (fmap convert' map)


check :: P1.Memory -> P2.Memory -> Int -> [Maybe (P2.Memory, P2.Memory)]
check prog1 prog2 steps = map diff $ take steps $ zip mem1's mem2s
    where
    mem1's = map convert (P1.memories prog1)
    mem2s = P2.memories prog2

diff (a,b) = if a == b then Nothing else Just (a,b)