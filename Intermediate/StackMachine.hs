module Intermediate.StackMachine (evaluate) where

import Intermediate.Model (SKI(S,K,I,T,L))

data Stack = Stack {base :: Ptr, count :: Int} deriving Show

-- We're using a very wasteful non-freeing heap (for demo purposes)
data Heap = Heap {tip :: Ptr}

-- This is where the Intermediate model separates from the
-- real model. Memory is external, not internal.
data State = State {memory :: Memory, stack :: Stack, heap :: Heap, current :: SKI} | Terminal deriving Show


push :: SKI -> Stack -> Memory -> (Stack, Mem)
push val (Stack base count) (Memory map) = (Stack (succ base) (succ count), Memory $ insert (succ base) val map)

step :: State -> State
step (State mem stack heap current) = case current of
    T a b -> State mem' stack' heap (deref a)
        where
        (stack', mem') = push (deref b) stack mem
    I     -> State mem stack' heap popped
        where
        (stack', popped) = pop stack mem
    K     -> State mem stack' head popped
        where
        (stack', _, popped) = pop2 stack mem 
    where
    deref ptr = let (Memory map) = mem in map ! ptr


step (State stack         (T a b)) = (State (b : stack)           a, Nothing)
step (State (s:stack)     I      ) = (State stack                 s, Nothing)
step (State (x:y:stack)   K      ) = (State stack                 x, Nothing)
step (State (x:y:z:stack) S      ) = (State (z : (T y z) : stack) x, Nothing)
step (State (s:stack)     (L l)  ) = (State stack                 s, Just l)
step (State []            (L l)  ) = (Terminal                     , Just l)

terminal :: State -> Bool
terminal Terminal = True
terminal _        = False

initial :: SKI -> State
initial ski = State [] ski

takeUntil f [] = []
takeUntil f (x:xs) = if f x then [x] else x : takeUntil f xs

steps :: SKI -> [(State, Maybe Char)]
steps program = takeUntil (terminal . fst) $ iterate (\(state,out) -> step state) $ (initial program, Nothing)

evaluate :: SKI -> [Char]
evaluate program = [char | (state, Just char) <- steps program]



