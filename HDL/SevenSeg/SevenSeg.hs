{-# LANGUAGE BinaryLiterals #-}

module SevenSeg (topEntity) where

import CLaSH.Prelude

{-
   A
   _
 F| |B
   - G
 E|_|C .H
   D
-}

next :: Index 4 -> Index 4
next 3 = 0
next n = n + 1

anode :: Index 4 -> BitVector 4
anode 0 = 0b0001
anode 1 = 0b0010
anode 2 = 0b0100
anode 3 = 0b1000

convert :: Unsigned 32 -> BitVector 8
convert 104 = 0b00101110 -- 'h'
convert 101 = 0b11011110 -- 'e'
convert 108 = 0b00001100 -- 'l'
convert 111 = 0b00111010 -- 'o'
convert 95  = 0b00010000 -- '_'
convert 119 = 0b01010100 -- 'w'
convert 114 = 0b10001100 -- 'r'
convert 100 = 0b01111010 -- 'd'
convert 33  = 0b01000001 -- '!'
convert _   = 0

display :: Signal (Vec 4 (Unsigned 32)) -> Signal (BitVector 4, BitVector 8)
display outputs = bundle (anode <$> active, cathodes)
    where
    cathodes = (xor 0xFF . convert) <$> liftA2 (!!) outputs active
    active = register 0 (next <$> active)

topEntity = display
