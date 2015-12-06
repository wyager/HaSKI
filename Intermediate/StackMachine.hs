module Intermediate.StackMachine (evaluate, steps, initial) where

import Prelude hiding (read)

import Intermediate.Model (Ptr(..), SKI(S,K,I,T,L))

import Intermediate.Memory (Memory, set, clear, read)

data Stack = Stack {base :: Ptr, count :: Word} deriving Show

-- We're using a very wasteful non-freeing heap (for demo purposes)
data Heap = Heap {tip :: Ptr} deriving Show

-- This is where the Intermediate model separates from the
-- real model. Memory is external, not internal.
data State = State {memory :: Memory, stack :: Stack, heap :: Heap, current :: SKI} | Terminal deriving Show

push :: SKI -> Stack -> Memory -> (Stack, Memory)
push val (Stack base count) memory = (Stack (succ base) (succ count), set memory (succ base) val)

push2 :: SKI -> SKI -> Stack -> Memory -> (Stack, Memory)
push2 a b stack memory = uncurry (push a) $ push b stack memory

pop :: Stack -> Memory -> (Stack, SKI)
pop (Stack base count) memory = (Stack (pred base) (pred count), read memory base)

pop2 :: Stack -> Memory -> (Stack, SKI, SKI)
pop2 stack memory = (stack'', a, b)
    where
    (stack',  a) = pop stack  memory
    (stack'', b) = pop stack' memory

pop3 :: Stack -> Memory -> (Stack, SKI, SKI, SKI)
pop3 stack memory = (stack'', a, b, c)
    where
    (stack', a, b) = pop2 stack memory
    (stack'', c)   = pop stack' memory


store :: Heap -> Memory -> SKI -> (Heap, Memory, Ptr)
store (Heap tip) memory ski = (Heap (pred tip), set memory tip ski, tip)

store2 :: Heap -> Memory -> SKI -> SKI -> (Heap, Memory, Ptr, Ptr)
store2 heap memory a b = (heap'', memory'', aptr, bptr)
    where
    (heap',  memory',  aptr) = store heap  memory  a
    (heap'', memory'', bptr) = store heap' memory' b

step :: State -> State
step (State mem stack heap current) = case current of
    T a b -> State mem' stack' heap (deref a)
        where
        (stack', mem') = push (deref b) stack mem
    I     -> State mem stack' heap popped
        where
        (stack', popped) = pop stack mem
    K     -> State mem stack' heap popped
        where
        (stack', popped, _) = pop2 stack mem 
    S     -> State mem'' stack'' heap' x
        where
        (stack', x, y, z) = pop3 stack mem
        (heap', mem', y_ptr, z_ptr) = store2 heap mem y z 
        (stack'', mem'') = push2 z (T y_ptr z_ptr) stack' mem'
    L c   -> case count stack of
        0 -> Terminal
        _ -> State mem stack' heap popped
        where
        (stack', popped) = pop stack mem
    where
    deref :: Ptr -> SKI
    deref = read mem

output :: State -> Maybe Char
output Terminal = Nothing
output state = case current state of
    L c -> Just c
    _   -> Nothing

terminal :: State -> Bool
terminal Terminal = True
terminal _        = False

initial :: Memory -> State
initial memory = State memory (Stack (Ptr 0x100000) 0) (Heap (Ptr maxBound)) (read memory (Ptr 0))

takeUntil f [] = []
takeUntil f (x:xs) = if f x then [x] else x : takeUntil f xs

steps :: Memory -> [State]
steps program = takeUntil terminal $ iterate step $ initial program

evaluate :: Memory -> [Char]
evaluate program = [char | (Just char) <- map output $ steps program]



