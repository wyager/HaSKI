module Hardware.Sim.Harness (evaluate) where

import CLaSH.Prelude hiding (read)

import Hardware.Model (Ptr(..), Output, binarize, unbinarize)

import qualified Hardware.Model as Model

import Hardware.Sim.Memory (RAMState, Memory, RAMStatus'(..), RAMAction'(..), memulate)

import Hardware.MMU (RAMStatus(..), RAMAction(..))

import Hardware.CPU (CPUState, cpu)

import Text.Printf (printf)

-- Convert from SKI terms to machine words before storing
serialize :: RAMAction -> RAMAction'
serialize (R p)   = R' p
serialize (W p s) = W' p $ Model.W (binarize s)
serialize X       = X'

-- Convert from machine words to SKI terms after loading
unserialize :: RAMStatus' -> RAMStatus
unserialize NoUpdate'                   = NoUpdate
unserialize (ReadComplete' (Model.W w)) = ReadComplete (unbinarize w)
unserialize WriteComplete'              = WriteComplete


evaluate :: Memory -> Signal (Maybe Output)
evaluate program = bundle (outputs)
    where
    (actions, outputs) = unbundle $ cpu (unserialize <$> mem_reads)
    (mem_reads, mem)  = unbundle $ memulate program (serialize <$> actions)
