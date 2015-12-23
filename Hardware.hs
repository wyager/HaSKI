module Hardware (topEntity) where

import CLaSH.Prelude
import Hardware.CPU (cpu)
import Hardware.MMU (RAMStatus, RAMAction)
import Hardware.Model (Output)

topEntity :: Signal RAMStatus -> Signal (RAMAction, Maybe Output)
topEntity = cpu
