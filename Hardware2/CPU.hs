module Hardware2.CPU () where

import Hardware2.StackMachine (State(Initializing))

import Hardware2.Model (Output(..))

cpu :: Signal RAMStatus -> (Signal RAMAction, Signal Output)
cpu ramstatus = 
    where
    state :: Signal State
    state = register initializing state'
    