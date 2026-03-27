{-# LANGUAGE FlexibleContexts #-}
module Hardware.MemoryEmulator.RAM (ram) where

import Clash.Prelude

type RAMStatusBits = (Bit, Bit, BitVector 64) -- Enable, read/write, data
type RAMActionBits = (Bit, Bit, BitVector 30, BitVector 64) -- Enable, read/write, ptr, data

ram :: (HiddenClockResetEnable System, KnownNat n)
    => Vec n (BitVector 64)
    -> Signal System RAMActionBits
    -> Signal System RAMStatusBits
ram initial requests = bundle (en', write', val)
    where
    (en, write, ptr, writeVal) = unbundle requests
    -- The block RAM takes 1 cycle to process, so we keep the request info
    -- for 1 cycle. If we weren't using block RAM, we might need more
    -- complicated logic.
    en' = register 0 en
    write' = register 0 write
    -- Modern Clash blockRam takes a Maybe (addr, data) for writes instead
    -- of separate write-addr/write-enable/write-data signals. We combine
    -- the original en && write condition with the ptr and data into a Maybe.
    mkWrite e w p v
      | bitToBool e && bitToBool w = Just (p, v)
      | otherwise                  = Nothing
    writeM = mkWrite <$> en <*> write <*> ptr <*> writeVal
    val = blockRam initial ptr writeM
