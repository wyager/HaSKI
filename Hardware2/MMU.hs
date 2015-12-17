module Hardware2.MMU () where

import CLaSH.Prelude

import Hardware2.Model (SKI,Ptr)

import Hardware2.Defs (MemRequest(..),MemResponse(..),Some(Zero,One,Two),Write(..),Read(..)) 

data Pending = Pending Reading Reading Writing Writing

data Reading = NotReading | Reading Ptr | DidRead SKI

data Writing = NotWriting | Writing Ptr SKI | Written

data RAMStatus = NoUpdate | ReadComplete SKI | WriteComplete

data RAMAction = R Ptr | W Ptr SKI | X

-- Take a fresh MemRequest and turn it into a pending memory operation.
initiate :: MemRequest -> Pending
initiate (MemRequest reads writes) = Pending r1 r2 w1 w2
    where
    (r1,r2) = case reads of
        Zero -> (NotReading, NotReading)
        One (Read pa) -> (Reading pa, NotReading)
        Two (Read pa) (Read pb) -> (Reading pa, Reading pb)
    (w1,w2) = case writes of
        Zero -> (NotWriting, NotWriting)
        One (Write va pa) -> (Writing pa va, NotWriting)
        Two (Write va pa) (Write vb pb) -> (Writing pa va, Writing pb vb)

-- Calculate the next action required to satisfy the memory operation.
next :: Pending -> RAMAction
next pending = case pending of
    Pending (Reading ptr) _ _ _ -> R ptr
    Pending _ (Reading ptr) _ _ -> R ptr
    Pending _ _ (Writing p x) _ -> W p x
    Pending _ _ _ (Writing p x) -> W p x
    Pending _ _ _ _             -> X

-- Given the current pending request, as well as the state of RAM 
-- (e.g. waiting for read to complete), generate the new pending state.
service :: Pending -> RAMStatus -> Pending
service pending NoUpdate = pending
service pending (ReadComplete ski) = case pending of
    Pending (Reading _) n o p -> Pending (DidRead ski) n o p
    Pending (DidRead m) (Reading _) o p -> Pending (DidRead m) (DidRead ski) o p
    _ -> error "Unexpected ReadComplete"
service pending WriteComplete = case pending of
    Pending (Reading _) _ _ _ -> error "WriteComplete before reading finished"
    Pending _ (Reading _) _ _ -> error "WriteComplete before reading finished (2)"
    Pending m n (Writing _ _) p -> Pending m n Written p
    Pending m n Written (Writing _ _) -> Pending m n Written Written
    _ -> error "Unexpected WriteComplete"

-- Determine if the pending request has finished.
-- If it has, return a MemResponse.
check :: Pending -> Maybe MemResponse
check (Pending r1 r2 w1 w2) = if doneWriting
    then MemResponse <$> reads
    else Nothing
    where
    doneWriting = case (w1,w2) of
        (Writing _ _, _) -> False
        (_, Writing _ _) -> False
        _                -> True
    reads = case (r1,r2) of
        (NotReading, NotReading) -> Just $ Zero
        (DidRead v1, NotReading) -> Just $ One v1
        (DidRead v1, DidRead v2) -> Just $ Two v1 v2
        _                        -> Nothing