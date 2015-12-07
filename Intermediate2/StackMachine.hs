module Intermediate2.StackMachine (evaluate, steps, initial) where

import Prelude hiding (read)

import Intermediate2.Model (Ptr(..), SKI(S,K,I,T,L))

import Intermediate2.Memory (Memory, set, clear, read)

import Text.Printf (printf)

-- Count is number of items in-memory
-- As many items as possible will be in the cache
data Stack = Stack {cache :: SKIs, base :: Ptr, count :: Word}
instance Show Stack where
    show (Stack cache (Ptr base) count) = printf "Stack@%08x [%d in memory] {%s}" base count (show cache)

data SKIs = None | One SKI | Two SKI SKI | Three SKI SKI SKI deriving Show

-- We're using a very wasteful non-freeing heap (for demo purposes)
data Heap = Heap {tip :: Ptr}
instance Show Heap where
    show (Heap (Ptr tip)) = printf "Heap@%08x" tip

-- This is where the Intermediate model separates from the
-- real model. Memory is external, not internal.
data State = State {stack :: Stack, heap :: Heap, current :: SKI} | Terminal deriving Show


--push :: SKI -> Stack -> Memory -> (Stack, Memory)
--push val (Stack base count) memory = (Stack (succ base) (succ count), set memory (succ base) val)

--push2 :: SKI -> SKI -> Stack -> Memory -> (Stack, Memory)
--push2 a b stack memory = uncurry (push a) $ push b stack memory

--pop :: Stack -> Memory -> (Stack, SKI)
--pop (Stack base count) memory = (Stack (pred base) (pred count), read memory base)

--pop2 :: Stack -> Memory -> (Stack, SKI, SKI)
--pop2 stack memory = (stack'', a, b)
--    where
--    (stack',  a) = pop stack  memory
--    (stack'', b) = pop stack' memory

--pop3 :: Stack -> Memory -> (Stack, SKI, SKI, SKI)
--pop3 stack memory = (stack'', a, b, c)
--    where
--    (stack', a, b) = pop2 stack memory
--    (stack'', c)   = pop stack' memory

--store :: Heap -> Memory -> SKI -> (Heap, Memory, Ptr)
--store (Heap tip) memory ski = (Heap (pred tip), set memory tip ski, tip)

--store2 :: Heap -> Memory -> SKI -> SKI -> (Heap, Memory, Ptr, Ptr)
--store2 heap memory a b = (heap'', memory'', aptr, bptr)
--    where
--    (heap',  memory',  aptr) = store heap  memory  a
--    (heap'', memory'', bptr) = store heap' memory' b


data Write = Write SKI Ptr 

data MemRequest = Deref2NoPush' Ptr Ptr -- Deref 2 pointers, push nothing onto stack
                | Deref2' Ptr Ptr Write -- Deref 2 pointers, push one term onto stack
                | MemStackEmpty' -- No-op, effectively. We're popping, but nothing on mem stack
                | MemStackHead' Ptr -- Deref one pointer (stack head)
                | MemStackTwo' Ptr Ptr -- Deref two pointers (top 2 of stack)
                | SPopMemStackEmpty' Write Write -- Store two things in the heap, pop nothing
                | SPopMemStackNonEmpty' Write Write Ptr -- Store two things in the heap, pop one thing

data MemResponse = Deref2 SKI SKI -- The response to a deref request from a T
                 | MemStackEmpty -- We're popping from the stack, but there's nothing in RAM
                 | MemStackHead SKI -- We're popping from the stack, and there's an element in RAM
                 | MemStackTwo SKI SKI -- We're popping two from the stack, and two are available in RAM
                 | SPopMemStackEmpty -- We stored two things in the heap, but couldn't pop anything from RAM
                 | SPopMemStackNonEmpty SKI -- We stored two things in the heap and popped one thing from RAM


serviceRequest :: Memory -> MemRequest -> MemResponse
serviceRequest mem req = case req of
    Deref2NoPush' a b             -> Deref2 (read mem a) (read mem b)
    Deref2' a b _                 -> Deref2 (read mem a) (read mem b)
    MemStackEmpty'                -> MemStackEmpty
    MemStackHead' ptr             -> MemStackHead (read mem ptr)
    MemStackTwo' a b              -> MemStackTwo (read mem a) (read mem b)
    SPopMemStackEmpty' _ _        -> SPopMemStackEmpty
    SPopMemStackNonEmpty' _ _ ptr -> SPopMemStackNonEmpty (read mem ptr)

memUpdate :: Memory -> MemRequest -> Memory
memUpdate mem req = case req of
    Deref2' _ _ (Write x ptr)                         -> set mem ptr x
    SPopMemStackEmpty' (Write a ap) (Write b bp)      -> set (set mem ap a) bp b
    SPopMemStackNonEmpty' (Write a ap) (Write b bp) _ -> set (set mem ap a) bp b
    _                                                 -> mem

step :: (State, Memory) -> (State, Memory)
step (state, mem) = (state', mem')
    where
    req = step1 state
    resp = serviceRequest mem req
    mem' = memUpdate mem req
    state' = step2 state resp

-- We have to decompose step into two steps:
-- One to figure out what we need from memory
-- and one to take the response from memory and process it.
-- We also need to cache 3 stack entries
step1 :: State -> MemRequest
step1 (State stack heap current) = case current of
    T a b -> case cache stack of
        Three x y z -> Deref2' a b $ Write z (base stack)
        _           -> Deref2NoPush' a b
    I -> case count stack of
        0 -> MemStackEmpty'
        _ -> MemStackHead' (pred $ base stack)
    K -> case count stack of
        0 -> MemStackEmpty'
        1 -> MemStackHead' (pred $ base stack)
        _ -> MemStackTwo' (pred $ base stack) (pred $ pred $ base stack)
    S -> case count stack of
        0 -> SPopMemStackEmpty' write1 write2
        _ -> SPopMemStackNonEmpty' write1 write2 (pred $ base stack)
        where
        Three _ y z = cache stack
        write1 = Write y (tip heap)
        write2 = Write z (pred $ tip heap)
    L _ -> case count stack of
        0 -> MemStackEmpty'
        _ -> MemStackHead' (pred $ base stack)

step2 :: State -> MemResponse -> State
step2 (State stack heap current) response = case (current, response) of
    (T _a _b, Deref2 a b) -> State (cachePush1 b stack) heap a
    (I,       pop)         -> case pop of
        MemStackEmpty        -> State (cachePop1NonFull stack) heap (cacheHead stack)
        MemStackHead memHead -> State (cachePop1Full stack memHead) heap (cacheHead stack)
    (K,       pop)         -> case pop of
        MemStackEmpty              -> State (cachePop2NonFull stack) heap (cacheHead stack)
        MemStackHead memHead       -> State (cachePop2OneFull stack memHead) heap (cacheHead stack)
        MemStackTwo memHead memSnd -> State (cachePop2Full stack memHead memSnd) heap (cacheHead stack)
    (S,       pop)         -> case pop of
        SPopMemStackEmpty -> State (pop3Push2EmptyMem stack y_ptr z_ptr) (store2 heap) (cacheHead stack)
        SPopMemStackNonEmpty memHead -> State (pop3Push2NonEmptyMem stack y_ptr z_ptr memHead) (store2 heap) (cacheHead stack)
        where
        y_ptr = tip heap
        z_ptr = pred y_ptr
    (L _,     pop)         -> case cache stack of
        None -> Terminal -- Nothing left to do
        _    -> case pop of
            MemStackEmpty        -> State (cachePop1NonFull stack) heap (cacheHead stack)
            MemStackHead memHead -> State (cachePop1Full stack memHead) heap (cacheHead stack)


-- Popping
shiftOnR :: SKIs -> SKI -> SKIs
shiftOnR skis ski = case skis of
    None -> One ski
    One a -> Two a ski
    Two a b -> Three a b ski

shiftOffL :: SKIs -> SKIs
shiftOffL (One a) = None
shiftOffL (Two a b) = One b
shiftOffL (Three a b c) = Two b c

-- Pushing
shiftOnL :: SKI -> SKIs -> SKIs
shiftOnL ski skis = case skis of
    None -> One ski
    One a -> Two ski a
    Two a b -> Three ski a b

shiftOffR :: SKIs -> SKIs
shiftOffR (One a) = None
shiftOffR (Two a b) = One a
shiftOffR (Three a b c) = Two a b

store2 :: Heap -> Heap
store2 (Heap tip) = Heap (pred $ pred tip)

pop3Push2NonEmptyMem :: Stack -> Ptr -> Ptr -> SKI -> Stack
pop3Push2NonEmptyMem (Stack cache base 0) _ _ _ = error "Using pop3Push2NonEmptyMem when mem is empty"
pop3Push2NonEmptyMem (Stack (Three _ y z) base n) y_ptr z_ptr memHead = Stack (Three z (T y_ptr z_ptr) memHead) (pred base) (pred n)

-- This corresponds to an S when there are exactly 3 items in the stack
pop3Push2EmptyMem :: Stack -> Ptr -> Ptr -> Stack
pop3Push2EmptyMem (Stack (Three x y z) base 0) y_ptr z_ptr = Stack (Two z (T y_ptr z_ptr)) base 0


-- Popping two when there are 2 or more in RAM stack
cachePop2Full :: Stack -> SKI -> SKI -> Stack
cachePop2Full (Stack cache base 0) _ _ = error "Empty mem stack in cachePop2NonFull"
cachePop2Full (Stack cache base 1) _ _ = error "1 mem stack in cachePop2NonFull"
cachePop2Full (Stack cache base n) memHead memSnd = Stack cache' (pred $ pred base) (pred $ pred n)
    where cache' = shiftOnR (shiftOnR (shiftOffL $ shiftOffL cache) memHead) memSnd

-- Popping two when there is exactly 1 in RAM stack
cachePop2OneFull :: Stack -> SKI ->  Stack
cachePop2OneFull (Stack cache base 1) head = Stack (shiftOnR (shiftOffL $ shiftOffL cache) head) (pred base) 0 -- Is this right? I'm a few beers in

-- Popping two when there is *no* memory stack
cachePop2NonFull :: Stack -> Stack
cachePop2NonFull (Stack cache base 0) = Stack (shiftOffL $ shiftOffL cache) base 0

cacheHead :: Stack -> SKI
cacheHead (Stack cache _ _) = case cache of
    One a -> a
    Two a _ -> a 
    Three a _ _ -> a

-- Using this implies we popped from cache when there *were* things
-- sitting in stack memory yet to be popped.
cachePop1Full :: Stack -> SKI -> Stack
cachePop1Full (Stack cache base 0) _ = error "Incorrect use of cachePop1Full"
cachePop1Full (Stack cache base cnt) head = Stack (shiftOnR (shiftOffL cache) head) (pred base) (pred cnt)

-- Using this implies that we didn't pop anything from RAM
-- because there's nothing left in RAM. Therefore, count *must* be 0.
cachePop1NonFull :: Stack -> Stack
cachePop1NonFull (Stack cache base 0) = Stack (shiftOffL cache) base 0

-- We stored 2 things in memory.
dec2 :: Heap -> Heap
dec2 (Heap tip) = Heap (pred $ pred tip)

-- The assumption is that, if the cache is full,
-- the tail of the cache has already been RAM'd
cachePush1 :: SKI -> Stack -> Stack
cachePush1 ski (Stack (Three a b c) base n) = Stack (Three ski a b) (succ base) (succ n)
cachePush1 ski (Stack cache base n) = Stack (shiftOnL ski cache) base n

output :: State -> Maybe Char
output Terminal = Nothing
output state = case current state of
    L c -> Just c
    _   -> Nothing

terminal :: State -> Bool
terminal Terminal = True
terminal _        = False

initial :: Memory -> State
initial memory = State (Stack None (Ptr 0x100000) 0) (Heap (Ptr 0x200000)) (read memory (Ptr 0))

takeUntil f [] = []
takeUntil f (x:xs) = if f x then [x] else x : takeUntil f xs

steps :: Memory -> [(State, Memory)]
steps program = takeUntil (terminal . fst) $ iterate step $ (initial program, program)

evaluate :: Memory -> [Char]
evaluate program = [char | (Just char) <- map (output . fst) $ steps program]
