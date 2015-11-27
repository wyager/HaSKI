module Intermediate.Memory (Memory, fromStack) where

import Intermediate.Model (Ptr(..), SKI)

import Data.Map.Strict (Map, fromList)

data Memory = Memory (Map Ptr SKI) deriving Show

fromStack :: [SKI] -> Memory
fromStack = Memory . fromList . zip [Ptr i | i <- [0..]] . reverse
