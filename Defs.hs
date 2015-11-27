{-# LANGUAGE ScopedTypeVariables #-}
module Defs (Ptr(..),SKI(..),Sized,Serialize,unserialize,serialize) where

import CLaSH.Prelude

data Ptr a = Ptr (Unsigned 30) deriving (Eq, Show)
data SKI = S | K | I | T (Ptr SKI) (Ptr SKI) | L (Signed 8) deriving (Show)


class Sized a where
    size :: Ptr a


sizeOf :: Sized a => a -> Ptr a
sizeOf _ = size

instance forall a . Sized a => Enum (Ptr a) where
    succ (Ptr a) = let Ptr sz = size :: Ptr a in Ptr (a + sz)
    pred (Ptr a) = let Ptr sz = size :: Ptr a in Ptr (a - sz)
    toEnum x     = let Ptr sz = size :: Ptr a in Ptr $ (toEnum x) * sz
    fromEnum     = undefined

instance Sized SKI where
    size = Ptr 8

class Serialize a where
    unserialize :: BitVector 64 -> a
    serialize :: a -> BitVector 64

instance Serialize SKI where
    serialize ski = tag ++# dataField
        where
        tag :: BitVector 4
        tag = case ski of
            S     -> 0
            K     -> 1
            I     -> 2
            T _ _ -> 3
            L _   -> 4
        dataField :: BitVector 60
        dataField = case ski of
            T (Ptr a) (Ptr b) -> pack a ++# pack b
            L c               -> pack c ++# 0
            _                 -> 0
    unserialize vec = case tag of
        0 -> S
        1 -> K
        2 -> I
        3 -> T ptr1 ptr2
        4 -> L word
        where
        tag = slice d63 d60 vec
        ptr1 = Ptr $ unpack (slice d59 d30 vec)
        ptr2 = Ptr $ unpack (slice d29 d0 vec)
        word = unpack $ slice d59 d52 vec
