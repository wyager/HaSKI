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
import Hardware.MemoryEmulator.RAM (ram)
import Hardware.MemoryEmulator.Default (defaultContents)

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
cpuHardware :: Signal RAMStatusBits -> Signal (RAMActionBits, OutputBits)
cpuHardware = fmap (ramaction *** output) . cpu . fmap ramstatus

-- NB: You wouldn't use this in a real design. You would use some sort
-- of external RAM device. However, doing this varies widely across FPGA
-- families, so I "cheated" and I am implementing RAM using a big register.
-- That way, this design should hopefully be more "plug-and-play".
-- If you'd like, you can replace this with actual RAM hardware for your FPGA.
-- This project uses 30-bit pointers to 64-bit words, so you could potentially
-- use up to 8GiB without much trouble.
ramHardware :: Signal RAMActionBits -> Signal RAMStatusBits
ramHardware = ram defaultContents

-- The thing that gets synthesized to an HDL
topEntity :: Signal OutputBits
topEntity = output
    where
    (ramRequest, output) = unbundle $ cpuHardware ramResponse
    ramResponse = ramHardware ramRequest
