{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Hardware.Defs (MemRequest(..),MemResponse(..),Some(Zero,One,Two),Write(..),Read(..)) where

import Clash.Prelude hiding (Read)

import Hardware.Model (SKI, Ptr)

-- We will never need more than two reads or two writes at once.
data MemRequest = MemRequest (Some Read) (Some Write) deriving (Show, Generic, NFDataX)

-- Return the requested Read results.
data MemResponse = MemResponse (Some SKI) deriving (Generic, NFDataX)

-- 0 to 2 of something
data Some x = Zero | One x | Two x x deriving (Show, Generic, NFDataX)

-- A memory write
data Write = Write SKI Ptr deriving (Show, Generic, NFDataX)

-- A memory read
data Read = Read Ptr deriving (Show, Generic, NFDataX)
