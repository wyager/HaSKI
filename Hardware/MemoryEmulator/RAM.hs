module Hardware.MemoryEmulator.RAM (
    ram
) where

import CLaSH.Prelude

type RAMStatusBits = (Bit, Bit, BitVector 64) -- Enable, read/write, data
type RAMActionBits = (Bit, Bit, BitVector 30, BitVector 64) -- Enable, read/write, ptr, data

ram :: KnownNat n => Vec n (BitVector 64) -> Signal RAMActionBits -> Signal RAMStatusBits
ram initial requests = response
    where
    -- Impose a bit of a delay
    active = register (0,0,0,0) $ register (0,0,0,0) $ requests
    mem = register initial mem'
    (mem', response) = unbundle $ handle <$> mem <*> active

handle :: KnownNat n => Vec n (BitVector 64) -> RAMActionBits -> (Vec n (BitVector 64), RAMStatusBits)
handle ram (0,_,_,_) = (ram, (0,0,0))
handle ram (1,0,ptr,_) = (ram, (1,0, ram !! ptr))
handle ram (1,1,ptr,ski) = (replace ptr ski ram, (1,1,0))
