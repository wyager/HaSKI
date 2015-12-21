{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hardware2.Model (
    Ptr(Ptr), SKI(S,K,I,T,L), Output(Output), W,
    binarize, unbinarize
) where

import CLaSH.Prelude

import Text.Printf (printf)

-- Machine word
data W = W (BitVector 64)

instance Show W where
    show (W v) = printf "[W 0x%02x 0x%08x 0x%08x]" tag a b
        where
        tag = toInteger $ slice d63 d60 v
        a   = toInteger $ slice d59 d30 v
        b   = toInteger $ slice d29 d0  v

-- 30-bit pointers to 64-bit words
-- Why? We want to fit two pointers plus 3 tag bits in a word.
-- This way, a whole SKI fits in a word.
newtype Ptr = Ptr (Unsigned 30) deriving (Show, Enum, Num)

-- SKI combinators.
-- We also have T (which represents the conjuction of two SKIs).
-- For example, the program (KSI) is represented as T (T K S) I.
-- We also have "L" combinators, which are like I except they produce output.
-- If a program terminates, it should terminate on an L.
data SKI = S | K | I | T Ptr Ptr | L Output deriving (Show)

-- 32-bit output values
data Output = Output (Unsigned 32) deriving (Show)

binarize :: SKI -> W
binarize ski = W $ case ski of
    S     -> tag 0 ++# 0
    K     -> tag 1 ++# 0
    I     -> tag 2 ++# 0
    T a b -> tag 3 ++# binarizePtr a ++# binarizePtr b
    L o   -> tag 4 ++# 0 ++# binarizeOutput o
    where
    -- Just makes it clear that the tag should be 4 bits long.
    tag :: BitVector 4 -> BitVector 4
    tag x = x

binarizePtr :: Ptr -> BitVector 30
binarizePtr (Ptr ptr) = pack ptr

binarizeOutput :: Output -> BitVector 32
binarizeOutput (Output o) = pack o

unbinarize :: W -> SKI
unbinarize (W w) = case tag of
    0 -> S
    1 -> K
    2 -> I
    3 -> T a b
    4 -> L o
    where
    tag = slice d63 d60 w
    a = unbinarizePtr $ slice d59 d30 w
    b = unbinarizePtr $ slice d29 d0 w
    o = unbinarizeOutput $ slice d31 d0 w

unbinarizePtr :: BitVector 30 -> Ptr
unbinarizePtr w = Ptr (unpack w)

unbinarizeOutput :: BitVector 32 -> Output
unbinarizeOutput w = Output (unpack w)
