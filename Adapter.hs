module Adapter (adapter, topEntity) where

import CLaSH.Prelude

import Hardware.Model (SKI(S,K,I,T,L), Ptr(..))

import Hardware.StackMachine (Write(..),run)

fromSKI :: SKI -> BitVector 64
fromSKI ski = case ski of
    S -> ix 0 ++# 0
    K -> ix 1 ++# 0
    I -> ix 2 ++# 0
    T a b -> ix 3 ++# trunc a ++# trunc b
    L c -> ix 4 ++# char c ++# 0
    where
    ix :: BitVector 4 -> BitVector 4
    ix = id

trunc :: Ptr -> BitVector 30
trunc (Ptr ptr) = toEnum ptr

toSKI :: BitVector 64 -> SKI
toSKI bv = case ix of
    0 -> S
    1 -> K
    2 -> I
    3 -> T (unTruncA) (unTruncB)
    4 -> L (unChar)
    where
    ix :: BitVector 4
    ix = slice d63 d60 bv
    unTruncA :: Ptr
    unTruncA = Ptr (fromEnum $ slice d59 d30 bv)
    unTruncB :: Ptr
    unTruncB = Ptr (fromEnum $ slice d29 d0 bv)
    unChar :: Char
    unChar = toEnum $ fromEnum $ slice d59 d52 bv

char :: Char -> BitVector 8
char = toEnum . fromEnum


a >$> b = b <$> a

adapter :: Signal (BitVector 64, BitVector 64) 
        -> Signal ((BitVector 30,                     BitVector 30), 
                   ((Bit, BitVector 30, BitVector 64), (Bit, BitVector 30, BitVector 64)),
                   (Bit, BitVector 8))
adapter mem_reads = bundle (reads', writes', output')
    where
    (reads, writes, output) = unbundle $ run mem_reads'
    mem_reads' = (\(a,b) -> (toSKI a, toSKI b)) <$> mem_reads
    reads' = (\(a,b) -> (trunc a, trunc b)) <$> reads
    writes' = (\(a,b) -> (formatWrite a, formatWrite b)) <$> writes
    output' = output >$> \x -> case x of
        Nothing -> (0,0)
        Just c -> (1, char c)
    formatWrite Nothing = (0,0,0)
    formatWrite (Just (Write ski ptr)) = (1,trunc ptr, fromSKI ski)

topEntity = adapter


-- run :: Signal (SKI,SKI) -> Signal((Ptr,Ptr), (Maybe Write, Maybe Write), Maybe Char)

