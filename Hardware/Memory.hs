module Hardware.Memory (Memory(..), fromStack, set, clear, read) where

import Prelude hiding (read)

import Hardware.Model (Ptr(..), SKI)

import Data.IntMap.Strict (IntMap, fromList, insert, delete, (!), toList, member)

import Text.Printf (printf)

import Data.List (intercalate)

data Memory = Memory (IntMap SKI) deriving (Eq)

fromStack :: [SKI] -> Memory
fromStack = Memory . fromList . zip [0..] . reverse

set :: Memory -> Ptr -> SKI -> Memory
set (Memory map) (Ptr ptr) ski = Memory $ insert ptr ski map

clear :: Memory -> Ptr -> Memory
clear (Memory map) (Ptr ptr) = Memory $ delete ptr map

read :: Memory -> Ptr -> SKI
read (Memory map) (Ptr ptr) = if ptr `member` map
    then map ! ptr
    else error ("Error: Invalid read pointer: " ++ show (Ptr ptr) ++ "\n" ++ show (Memory map))

instance Show Memory where
    show (Memory entries) = ("\n" ++) $ intercalate "\n" $ map showEntry $ toList entries

showEntry :: (Int, SKI) -> String
showEntry (addr,val) = printf "%08x: %s" addr (show val)