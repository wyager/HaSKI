{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Hardware.Model (
    Ptr(Ptr), SKI(S,K,I,T,L), Output(Output), W(W),
    binarize, unbinarize, binarizePtr
) where

import Clash.Prelude

import Text.Printf (printf)

-- Machine word
data W = W (BitVector 64)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

instance Show W where
    show (W v) = printf "[W 0x%02x 0x%08x 0x%08x]" tag a b
        where
        tag = toInteger $ slice d63 d60 v
        a   = toInteger $ slice d59 d30 v
        b   = toInteger $ slice d29 d0  v

-- 30-bit pointers to 64-bit words
-- Why? We want to fit two pointers plus 3 tag bits in a word.
-- This way, a whole SKI fits in a word.
newtype Ptr = Ptr (Unsigned 30)
    deriving newtype  (Show, Enum, Num)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

-- SKI combinators.
-- We also have T (which represents the conjuction of two SKIs).
-- For example, the program (KSI) is represented as T (T K S) I.
-- We also have "L" combinators, which are like I except they produce output.
-- If a program terminates, it should terminate on an L.
data SKI = S | K | I | T Ptr Ptr | L Output
    deriving stock    (Show, Generic)
    deriving anyclass (NFDataX)

-- 32-bit output values
data Output = Output (Unsigned 32)
    deriving stock    (Show, Generic)
    deriving anyclass (NFDataX)

binarize :: SKI -> BitVector 64
binarize ski = case ski of
    S     -> tag 0 ++# (0 :: BitVector 60)
    K     -> tag 1 ++# (0 :: BitVector 60)
    I     -> tag 2 ++# (0 :: BitVector 60)
    T a b -> tag 3 ++# binarizePtr a ++# binarizePtr b
    L o   -> tag 4 ++# (0 :: BitVector 28) ++# binarizeOutput o
    where
    -- Just makes it clear that the tag should be 4 bits long.
    tag :: BitVector 4 -> BitVector 4
    tag x = x

binarizePtr :: Ptr -> BitVector 30
binarizePtr (Ptr ptr) = pack ptr

binarizeOutput :: Output -> BitVector 32
binarizeOutput (Output o) = pack o

unbinarize :: BitVector 64 -> SKI
unbinarize w = case tag of
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
