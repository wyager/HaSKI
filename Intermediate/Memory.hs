module Intermediate.Memory (Memory, fromStack, set, clear, read) where

import Prelude hiding (read)

import Intermediate.Model (Ptr(..), SKI)

import Data.Map.Strict (Map, fromList, insert, delete, (!))

data Memory = Memory (Map Ptr SKI) deriving Show

fromStack :: [SKI] -> Memory
fromStack = Memory . fromList . zip [Ptr i | i <- [0..]] . reverse

set :: Memory -> Ptr -> SKI -> Memory
set (Memory map) ptr ski = Memory $ insert ptr ski map

clear :: Memory -> Ptr -> Memory
clear (Memory map) ptr = Memory $ delete ptr map

read :: Memory -> Ptr -> SKI
read (Memory map) ptr = map ! ptr