{-# LANGUAGE ScopedTypeVariables #-}
import CLaSH.Prelude hiding (shift, head, read)
import qualified Data.Map.Strict as M
main = print "ayy"


data Ptr a = Ptr (Signed 30) deriving (Eq, Show)

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

data SKI = S | K | I | T (Ptr SKI) (Ptr SKI) | L (Signed 8) deriving (Show)

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


data Heap a = Heap {base :: Ptr a, tip :: Ptr a} deriving (Show)
data Stack a = Stack {cache :: Elems a, head :: Ptr a, count :: Unsigned 32} deriving (Show)
data Elems a = None | One a | Two a a | Three a a a deriving (Show)
-- NB: Think more about if when I actually have to mess with head
push :: Sized a => a -> Stack a -> Stack a
push a (Stack cache head count) = Stack (shift a cache) (succ head) (count + 1)

shift :: a -> Elems a -> Elems a
shift a elems = case elems of
    None -> One a
    One x -> Two a x
    Two x y -> Three a x y
    Three x y z -> Three a x y


insert2 :: Sized a => Heap a -> Heap a
insert2 (Heap base tip) = Heap base (pred (pred tip))

-- The next thing to put into memory
next :: Stack a -> Maybe a
next (Stack None _ _) = Nothing
next (Stack (One x) _ _) = Just x
next (Stack (Two _ y) _ _) = Just y
next (Stack (Three _ _ z) _ _) = Just z

peek1 :: Stack a -> Maybe a
peek1 (Stack None _ _) = Nothing
peek1 (Stack (One x) _ _) = Just x
peek1 (Stack (Two x _) _ _) = Just x
peek1 (Stack (Three x _ _) _ _) = Just x


peek2 :: Stack a -> Maybe (a,a)
peek2 (Stack None _ _) = Nothing
peek2 (Stack (One _) _ _) = Nothing
peek2 (Stack (Two x y) _ _) = Just (x,y)
peek2 (Stack (Three x y _) _ _) = Just (x,y)

peek3 :: Stack a -> Maybe (a,a,a)
peek3 (Stack None _ _) = Nothing
peek3 (Stack (One _) _ _) = Nothing
peek3 (Stack (Two _ _) _ _) = Nothing
peek3 (Stack (Three x y z) _ _) = Just (x,y,z)


-- Need 2 read, 2 write port
-- Read/write profiles:
-- IPop reads 1
-- KPop reads 2
-- SPush reads 1, writes 2
-- TPush reads 2, writes 1 (read 2 from mem, write from back of stack cache)
-- LPop reads 1

-- IPop s ~      \(_:stack) -> stack                   ; s
-- KPop s ~      \(_:_:stack) -> stack                 ; s
-- SPush x y z ~ \(_:_:_:stack) -> z : (T y z) : stack ; x -- Requires putting y,z on heap
-- TPush a b ~   \(stack) -> *b : stack                ; *a
-- LPop s ~      \(_:stack) -> stack                   ; s
data MemoryAction = IPop SKI | KPop SKI | SPush SKI SKI SKI | TPush (Ptr SKI) (Ptr SKI) | LPop SKI deriving (Show)

data Output o = SomeOutput o | NoOutput deriving (Show)


-- Hmm. We need to be able to generate a Ptr for any SKI objects, or else
-- we can't push them in a T onto the stack.

-- I think we need dynamic memory for T. We have to push its arguments into memory.


actOn :: State -> MemoryAction
actOn (State ski stack _) = case ski of
    I -> case peek1 stack of
        Nothing         -> error "Unapplied I"
        Just s          -> IPop s
    K -> case peek2 stack of
        Nothing         -> error "Unapplied K"
        Just (x,y)      -> KPop x
    S -> case peek3 stack of
        Nothing         -> error "Unapplied S"
        Just (x,y,z)    -> SPush x y z
    T a b               -> TPush a b
    L c -> case peek1 stack of
        Nothing         -> LPop (L c)-- Just keep outputting c if nothing left
        Just s          -> LPop s

outputOf :: State -> Output (Signed 8)
outputOf (State ski _ _) = case ski of
    L c -> SomeOutput c
    _   -> NoOutput

popReadRequest :: Stack a -> ReadRequest a
popReadRequest (Stack cache head count) = if count > 3 
    then OneReadRequest head
    else NoReadRequest

popReadResponse :: Sized a => a -> Stack a -> Stack a
popReadResponse a (Stack cache head count) = if count > 3
    then case cache of
        Three x y z -> Stack (Three y z a) (pred head) (count - 1)
        _           -> error "Popping from non-saturated stack"
    else case cache of
        Three x y z -> Stack (Two y z) head (count - 1)
        Two x y -> Stack (One y) head (count - 1)
        One y -> Stack (None) head (count - 1)
        _  -> error "Popping from empty stack"


pop2ReadRequest :: Sized a => Stack a -> ReadRequest a
pop2ReadRequest (Stack cache head count)
    | count > 4 = TwoReadRequest head (pred head)
    | count > 3 = OneReadRequest head
    | otherwise = NoReadRequest

pop2ReadResponse :: Sized a => (a,a) -> Stack a -> Stack a
pop2ReadResponse (a,b) (Stack cache head count)
    | count > 4 = case cache of
        Three x y z -> Stack (Three z a b) (pred (pred head)) (count - 2)
        _           -> error "Popping 2 from non-saturated stack"
    | count > 3 = case cache of
        Three x y z -> Stack (Two z a) (pred head) (count - 2)
        _           -> error "Popping 2 from non-saturated stack (2)"
    | otherwise = case cache of
        Three x y z -> Stack (One z) head (count - 2)
        Two x y     -> Stack (None) head (count - 2)
        _           -> error "Popping 2 from stack without two elements"

pushWriteRequest :: Sized a => Stack a -> WriteRequest a
pushWriteRequest (Stack cache head count) = case cache of
    Three a b c -> OneWriteRequest (succ head) c
    _           -> NoWriteRequest

pushWriteResponse :: Sized a => a -> Stack a -> Stack a
pushWriteResponse x (Stack cache head count) = case cache of
    Three a b c -> Stack (Three x a b) (succ head) (count + 1)
    Two a b     -> Stack (Three x a b) head        (count + 1)
    One a       -> Stack (Two x a)     head        (count + 1)
    None        -> Stack (One x)       head        (count + 1)

pop3Push2ReadRequest :: Sized a => Stack a -> ReadRequest a
pop3Push2ReadRequest (Stack cache head count) = if count > 3
    then OneReadRequest head
    else NoReadRequest

pop3Push2ReadResponse :: Sized a => (a,a) -> a -> Stack a -> Stack a
pop3Push2ReadResponse (ga,gb) ma (Stack cache head count) = if count > 3
    then Stack (Three ga gb ma) (pred head) (count - 1)
    else Stack (Two ga gb)      head        (count - 1) 



data ReadRequest a = NoReadRequest | OneReadRequest (Ptr a) | TwoReadRequest (Ptr a) (Ptr a) deriving (Show)
data WriteRequest a = NoWriteRequest | OneWriteRequest (Ptr a) a | TwoWriteRequest (Ptr a) a (Ptr a) a deriving (Show)
memRequest :: State -> MemoryAction -> (ReadRequest SKI, WriteRequest SKI)
memRequest (State _ stack heap) action = case action of
    LPop _ -> (popReadRequest stack, NoWriteRequest)
    IPop _ -> (popReadRequest stack, NoWriteRequest)
    KPop _ -> (pop2ReadRequest stack, NoWriteRequest)
    SPush x y z -> (pop3Push2ReadRequest stack, TwoWriteRequest (pred (tip heap)) y (pred (pred (tip heap))) z)
    TPush a b -> (TwoReadRequest a b, pushWriteRequest stack)


memResponse :: State -> MemoryAction -> (SKI,SKI) -> State
memResponse (State _ stack heap) action (ra,rb) = case action of
    LPop ski -> State ski (popReadResponse ra stack) heap
    IPop ski -> State ski (popReadResponse ra stack) heap
    KPop ski -> State ski (pop2ReadResponse (ra,rb) stack) heap
    SPush x y z -> State x (pop3Push2ReadResponse (x, (T heapsnd heapfst)) ra stack) (insert2 heap)
    TPush a b -> State ra (pushWriteResponse rb stack) heap
    where
    heapfst = pred (tip heap)
    heapsnd = pred heapfst

process :: State -> (SKI,SKI) -> (State, ReadRequest SKI, WriteRequest SKI, Output (Signed 8))
process old_state mem_reads = (state', reads', writes', output')
    where
    state' = memResponse old_state (actOn old_state) mem_reads
    action' = actOn state'
    (reads', writes') = memRequest state' action'
    output' = outputOf state'


process' :: State -> Signal (SKI,SKI) -> Signal (ReadRequest SKI, WriteRequest SKI, Output (Signed 8), State)
process' state0 mem_reads = bundle (readReqs', writeReqs', output', state')
    where
    initialized = register False (signal True)
    state = regEn state0 initialized state' :: Signal State
    (state', readReqs', writeReqs', output') = unbundle $ process <$> state <*> mem_reads


run :: Memory -> State -> Signal (Output (Signed 8), State, Memory)
run mem0 state0 = bundle (output, state, mem)
    where
    mem = register mem0 (applyWriteReq <$> writeReqs <*> mem)
    reads = doReadReqs <$> readReqs <*> mem
    applyWriteReq NoWriteRequest mem = mem
    applyWriteReq (OneWriteRequest (Ptr p) a) mem = write mem p (serialize a)
    applyWriteReq (TwoWriteRequest (Ptr p1) a1 (Ptr p2) a2) mem = write (write mem p1 (serialize a1)) p2 (serialize a2)
    doReadReqs NoReadRequest mem = (undefined, undefined)
    doReadReqs (OneReadRequest (Ptr p)) mem = (unserialize (read mem p), undefined)
    doReadReqs (TwoReadRequest (Ptr p1) (Ptr p2)) mem = (unserialize (read mem p1), unserialize (read mem p2))
    (readReqs, writeReqs, output, state) = unbundle $ process' state0 reads


data State = State SKI (Stack SKI) (Heap SKI) deriving Show

data Memory = Memory (M.Map (Signed 30) (BitVector 8))

read :: Memory -> Signed 30 -> BitVector 64
read (Memory map) ptr = (map M.! ptr + 0) ++#
                        (map M.! ptr + 1) ++#
                        (map M.! ptr + 2) ++#
                        (map M.! ptr + 3) ++#
                        (map M.! ptr + 4) ++#
                        (map M.! ptr + 5) ++#
                        (map M.! ptr + 6) ++#
                        (map M.! ptr + 7)

write :: Memory -> Signed 30 -> BitVector 64 -> Memory
write (Memory map) ptr val = Memory $ 
                             M.insert (ptr + 0) (slice d63 d56 val) . 
                             M.insert (ptr + 2) (slice d55 d48 val) . 
                             M.insert (ptr + 3) (slice d47 d40 val) . 
                             M.insert (ptr + 4) (slice d39 d32 val) . 
                             M.insert (ptr + 5) (slice d31 d24 val) . 
                             M.insert (ptr + 6) (slice d23 d16 val) . 
                             M.insert (ptr + 7) (slice d15 d8  val) . 
                             M.insert (ptr + 7) (slice d7  d0  val) $ map

initial :: SKI -> Ptr SKI -> Ptr SKI -> State
initial ski sbase hbase = State ski (Stack None sbase 0) (Heap hbase hbase)


