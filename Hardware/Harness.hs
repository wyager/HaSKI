module Hardware.Harness (evaluate) where

import CLaSH.Prelude hiding (read)

import Hardware.Model (Ptr(..), SKI(S,K,I,T,L))

import Hardware.Memory (Memory, set, clear, read)

import Hardware.StackMachine (Write(..), State, run)

import Text.Printf (printf)

--serviceRequest :: Memory -> MemRequest -> MemResponse
--serviceRequest mem req = case req of
--    LoadMain' (Ptr 0)             -> LoadMain (read mem (Ptr 0))
--    Deref2NoPush' a b             -> Deref2 (read mem a) (read mem b)
--    Deref2' a b _                 -> Deref2 (read mem a) (read mem b)
--    MemStackEmpty'                -> MemStackEmpty
--    MemStackHead' ptr             -> MemStackHead (read mem ptr)
--    MemStackTwo' a b              -> MemStackTwo (read mem a) (read mem b)
--    SPopMemStackEmpty' _ _        -> SPopMemStackEmpty
--    SPopMemStackNonEmpty' _ _ ptr -> SPopMemStackNonEmpty (read mem ptr)

--memUpdate :: Memory -> MemRequest -> Memory
--memUpdate mem req = case req of
--    Deref2' _ _ (Write x ptr)                         -> set mem ptr x
--    SPopMemStackEmpty' (Write a ap) (Write b bp)      -> set (set mem ap a) bp b
--    SPopMemStackNonEmpty' (Write a ap) (Write b bp) _ -> set (set mem ap a) bp b
--    _                                                 -> mem


memulate :: Memory -> Signal (Ptr, Ptr) -> Signal (Maybe Write, Maybe Write) -> Signal (SKI, SKI)
memulate initial reads writes = output
    where
    output = register undefined (read2 <$> reads <*> mem)
    mem    = register initial (write2 <$> writes <*> mem)
    read2 (a,b) mem = (read mem a, read mem b)
    write2 (a,b) = write1 a . write1 b
    write1 write mem = case write of
        Nothing -> mem
        Just (Write ski ptr) -> set mem ptr ski

evaluate :: Memory -> Signal (Maybe Char)
evaluate program = outputs
    where
    (reads, writes, outputs) = unbundle $ run mem_reads
    mem_reads = memulate program reads writes

--terminal :: State -> Bool
--terminal Terminal = True
--terminal _        = False

--takeUntil f [] = []
--takeUntil f (x:xs) = if f x then [x] else x : takeUntil f xs

--steps :: Memory -> [(State, Memory)]
--steps program = takeUntil (terminal . fst) $ iterate step $ (Initialized, program)

--evaluate :: Memory -> [Char]
--evaluate program = [char | (Just char) <- map (output . fst) $ steps program]

--memories :: Memory -> [Memory]
--memories = map snd . steps
