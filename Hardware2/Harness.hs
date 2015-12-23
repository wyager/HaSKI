module Hardware2.Harness (evaluate) where

import CLaSH.Prelude hiding (read)

import Hardware2.Model (Ptr(..), W, Output, binarize, unbinarize)

import Hardware2.Memory (RAMState, Memory, RAMStatus'(..), RAMAction'(..), memulate)

import Hardware2.MMU (RAMStatus(..), RAMAction(..))

import Hardware2.CPU (CPUState, cpu)

import Text.Printf (printf)

-- Convert from SKI terms to machine words before storing
serialize :: RAMAction -> RAMAction'
serialize (R p)   = R' p
serialize (W p s) = W' p (binarize s)
serialize X       = X'

-- Convert from machine words to SKI terms after loading
unserialize :: RAMStatus' -> RAMStatus
unserialize NoUpdate'         = NoUpdate
unserialize (ReadComplete' w) = ReadComplete (unbinarize w)
unserialize WriteComplete'    = WriteComplete


evaluate :: Memory -> Signal (Maybe Output)
evaluate program = bundle (outputs)
    where
    (actions, outputs) = unbundle $ cpu (unserialize <$> mem_reads)
    (mem_reads, mem)  = unbundle $ memulate program (serialize <$> actions)
