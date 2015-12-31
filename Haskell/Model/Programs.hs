module Model.Programs where

import Model.Model (SKI)
import Model.Parser (parse)
import Model.StackMachine (evaluate)

hello4 :: SKI
hello4 = parse "SII(SII(hello_world!\n))"

ababs :: SKI
ababs = parse "SII(S(K(ab))(SII))"

double :: SKI
double = parse "SIIa"

swap :: SKI
swap = parse "S(K(SI))Kab"
