module Hardware2.CPU (cpu) where

import CLaSH.Prelude

import Hardware2.StackMachine (State(Initializing), step1, step2, outputOf)

import Hardware2.MMU (Pending, RAMStatus(NoUpdate), RAMAction(X),
    initiate, next, service, check)

import Hardware2.Model (Output(..))

-- We need to keep track of the evaluator state as well as the MMU state.
data CPUState = Startup | CPU State Pending

-- The state of the MMU when the CPU is initialized.
-- If you look at the definition of step1, this corresponds
-- to loading the SKI combinator from 0x0. This is where the
-- root of the program tree goes.
bootup :: Pending
bootup = initiate (step1 Initializing)

-- The transition function of the CPU.
step :: CPUState -> RAMStatus -> (CPUState, RAMAction, Maybe Output)
step Startup             NoUpdate = (CPU Initializing pending, next pending, Nothing)
    where
    pending = initiate (step1 Initializing)
step (CPU state pending) NoUpdate = (CPU state  pending,   X,      Nothing)
step (CPU state pending) update   = (CPU state' pending'', action, output)
    where
    pending' = service pending update
    result = check pending' -- Did we finish a transaction?
    state' = case result of
        Nothing     -> state -- Memory transaction did not finish.
        Just result -> step2 state result -- Finished! Calculate new state.
    pending'' = case result of
        Nothing -> pending' -- We can keep servicing this transaction
        Just _  -> initiate (step1 state') -- Start servicing new transaction.
    action = next pending''
    output = case result of
        Nothing -> Nothing -- We only output if we're in a brand new state
        Just _  -> outputOf state' -- New state, new output.

cpu :: Signal RAMStatus -> Signal (RAMAction, Maybe Output)
cpu ramstatus = bundle (action, output)
    where
    state  :: Signal CPUState
    state = register Startup state'
    state' :: Signal CPUState
    action :: Signal RAMAction
    output :: Signal (Maybe Output)
    (state', action, output) = unbundle $ step <$> state <*> ramstatus
