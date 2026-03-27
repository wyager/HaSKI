{-# LANGUAGE FlexibleContexts #-}
module Hardware (topEntity) where

import Clash.Prelude
import Hardware.CPU (cpu, Halt(DoHalt, Don'tHalt))
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
type HaltBit       = Bit

ramstatus :: RAMStatusBits -> RAMStatus
ramstatus (0,_,_) = NoUpdate
ramstatus (1,0,s) = ReadComplete (unbinarize s)
ramstatus (1,1,_) = WriteComplete
ramstatus _       = NoUpdate  -- unreachable, pacify -Wincomplete-patterns

ramaction :: RAMAction -> RAMActionBits
ramaction (R ptr)   = (1, 0, binarizePtr ptr, 0)
ramaction (W ptr x) = (1, 1, binarizePtr ptr, binarize x)
ramaction (X)       = (0, 0, 0,               0)

output :: Maybe Output -> OutputBits
output Nothing           = (0, 0)
output (Just (Output o)) = (1, o)

halt :: Halt -> HaltBit
halt DoHalt    = 1
halt Don'tHalt = 0

-- NB: cpu :: Signal RAMStatus -> Signal (RAMAction, Maybe Output, Halt)
cpuHardware :: HiddenClockResetEnable System
            => Signal System RAMStatusBits
            -> Signal System (RAMActionBits, OutputBits, HaltBit)
cpuHardware = fmap convert . cpu . fmap ramstatus
    where convert (ram,out,done) = (ramaction ram, output out, halt done)

-- NB: For portability, we are not using "real" external RAM; we are using
-- block RAM. Block RAM is fast single-cycle RAM built into the FPGA. See
-- the definition of "ram"; we're using Clash's blockRam primitive.
-- This generates code that most synthesis tools will recognize
-- as block RAM. Unfortunately, most FPGAs only have block RAM in the hundreds
-- of kilobits or low megabits.
-- If you'd like, you can replace this with actual RAM hardware for your FPGA.
-- This project uses 30-bit pointers to 64-bit words, so you could potentially
-- use up to 8GiB without much trouble. However, be warned that real RAM
-- tends to be much more complicated and platform-specific.
ramHardware :: HiddenClockResetEnable System
            => Signal System RAMActionBits -> Signal System RAMStatusBits
ramHardware = ram defaultContents

-- The core evaluator loop, factored out so topEntity can thread in
-- clock/reset via withClockResetEnable.
haski :: HiddenClockResetEnable System
      => Signal System (OutputBits, HaltBit)
haski = bundle (out, hlt)
    where
    (ramRequest, out, hlt) = unbundle $ cpuHardware ramResponse
    ramResponse = ramHardware ramRequest

-- The thing that gets synthesized to an HDL. Modern Clash requires
-- explicit Clock/Reset ports; enableGen suppresses the Enable port.
topEntity :: Clock System -> Reset System -> Signal System (OutputBits, HaltBit)
topEntity clk rst = withClockResetEnable clk rst enableGen haski
{-# ANN topEntity
  (Synthesize
    { t_name   = "haski"
    , t_inputs = [PortName "clk", PortName "rst"]
    , t_output = PortName "out"
    }) #-}
{-# NOINLINE topEntity #-}
