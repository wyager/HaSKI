module Hardware.MemoryEmulator.RAM (ram) where

import CLaSH.Prelude

type RAMStatusBits = (Bit, Bit, BitVector 64) -- Enable, read/write, data
type RAMActionBits = (Bit, Bit, BitVector 30, BitVector 64) -- Enable, read/write, ptr, data

bit2bool :: Bit -> Bool
bit2bool 1 = True
bit2bool 0 = False

bit2bool' :: Signal Bit -> Signal Bool
bit2bool' = fmap bit2bool

ram :: KnownNat n
    => Vec n (BitVector 64)
    -> Signal RAMActionBits
    -> Signal RAMStatusBits
ram initial requests = bundle (en', write', val)
    where
    (en, write, ptr, writeVal) = unbundle requests
    -- The block RAM takes 1 cycle to process, so we keep the request info
    -- for 1 cycle. If we weren't using block RAM, we might need more
    -- complicated logic.
    en' = register 0 en
    write' = register 0 write
    val = blockRam initial ptr ptr ((bit2bool' en) .&&. (bit2bool' write)) writeVal
