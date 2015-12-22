module Hardware2.Memory (memulate, fromStack, Memory, RAMState, RAMStatus'(..), RAMAction'(..)) where

import CLaSH.Prelude (Unsigned, Index, Signal, register, bundle)

import Prelude hiding (read)

import Hardware2.Model (Ptr(..), W)

import Data.Map.Strict (Map, fromList, insert, delete, (!), toList, member)

import Text.Printf (printf)

import Data.List (intercalate)

type U30 = Unsigned 30

data Memory = Memory (Map U30 W)

data State = Idle | Reading Ptr (Index 1) | Writing Ptr W (Index 1) deriving Show

data RAMState = RS State Memory deriving Show

data RAMStatus' = NoUpdate' | ReadComplete' W | WriteComplete'

data RAMAction' = R' Ptr     -- Read
                | W' Ptr W -- Write
                | X'         -- Nothing
                deriving Show

memulate :: Memory -> Signal RAMAction' -> Signal (RAMStatus', RAMState)
memulate initial actions = bundle (outputOf <$> mem, mem)
    where
    actions' = register X' actions
    mem = register (RS Idle initial) mem'
    mem' = step <$> mem <*> actions'

step :: RAMState -> RAMAction' -> RAMState
step (RS Idle mem) action = case action of
    R' ptr   -> RS (Reading ptr maxBound)   mem
    W' ptr w -> RS (Writing ptr w maxBound) mem
    X'       -> RS Idle                     mem
step (RS (Reading ptr 0)   mem)   X' = RS Idle mem
step (RS (Writing ptr w 0) mem)   X' = RS Idle (set mem ptr w)
step (RS (Reading ptr n)   mem)   X' = RS (Reading ptr (n-1))   mem
step (RS (Writing ptr w n) mem)   X' = RS (Writing ptr w (n-1)) mem
step state action = error $ printf "Unexpected memory state/action.\nState: %s\nAction: %s" (show state) (show action)

outputOf :: RAMState -> RAMStatus'
outputOf (RS (Reading ptr 0)   mem) = ReadComplete' (read mem ptr)
outputOf (RS (Writing ptr w 0) mem) = WriteComplete'
outputOf _                          = NoUpdate'

fromStack :: [W] -> Memory
fromStack = Memory . fromList . zip [0..]

set :: Memory -> Ptr -> W -> Memory
set (Memory map) (Ptr ptr) w = Memory $ insert ptr w map

clear :: Memory -> Ptr -> Memory
clear (Memory map) (Ptr ptr) = Memory $ delete ptr map

read :: Memory -> Ptr -> W
read (Memory map) (Ptr ptr) = if ptr `member` map
    then map ! ptr
    else error ("Error: Invalid read pointer: " ++ show (Ptr ptr) ++ "\n" ++ show (Memory map))

instance Show Memory where
    show (Memory entries) = ("\n" ++) $ intercalate "\n" $ map showEntry $ toList entries

showEntry :: (U30, W) -> String
showEntry (addr,val) = printf "%08x: %s" (fromEnum addr) (show val)
