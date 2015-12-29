module Hardware (topEntity) where

import CLaSH.Prelude
import Control.Arrow ((***))
import Hardware.CPU (cpu)
import Hardware.MMU (
    RAMStatus(NoUpdate, ReadComplete, WriteComplete),
    RAMAction(W,R,X) )
import Hardware.Model (
    Output, W, Ptr(Ptr), SKI, Output(..),
    binarize, unbinarize, binarizePtr )

-- topEntity :: Signal RAMStatus -> Signal (RAMAction, Maybe Output)
-- topEntity = cpu

type RAMStatusBits = (Bit, Bit, BitVector 64) -- Enable, read/write, data
type RAMActionBits = (Bit, Bit, BitVector 30, BitVector 64) -- Enable, read/write, ptr, data
type OutputBits    = (Bit, Unsigned 32) -- Enable, data

ramstatus :: RAMStatusBits -> RAMStatus
ramstatus (0,_,_) = NoUpdate
ramstatus (1,0,s) = ReadComplete (unbinarize s)
ramstatus (1,1,_) = WriteComplete

ramaction :: RAMAction -> RAMActionBits
ramaction (R ptr)   = (1, 0, binarizePtr ptr, 0)
ramaction (W ptr x) = (1, 1, binarizePtr ptr, binarize x)
ramaction (X)       = (0, 0, 0,               0)

output :: Maybe Output -> OutputBits
output Nothing           = (0, 0)
output (Just (Output o)) = (1, o)


-- NB: cpu :: Signal RAMStatus -> Signal (RAMAction, Maybe Output)
topEntity :: Signal RAMStatusBits -> Signal (RAMActionBits, OutputBits)
topEntity = fmap (ramaction *** output) . cpu . fmap ramstatus
