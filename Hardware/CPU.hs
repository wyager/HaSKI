module Hardware.CPU (CPUState, cpu) where

import CLaSH.Prelude

import Hardware.StackMachine (State(Initializing), step1, step2, outputOf)

import Hardware.MMU (Pending, RAMStatus(NoUpdate), RAMAction(X),
    initiate, next, service, check)

import Hardware.Model (Output(..))

-- Are we waiting for a single memory action (read/write/etc.) to complete?
data Waiting = No | Yes deriving Show

-- We need to keep track of the evaluator state as well as the MMU state.
data CPUState = CPU State Pending Waiting deriving Show

-- The state of the MMU when the CPU is initialized.
-- If you look at the definition of step1, this corresponds
-- to loading the SKI combinator from 0x0. This is where the
-- root of the program tree goes.
bootup :: Pending
bootup = initiate (step1 Initializing)

-- The transition function of the CPU.
-- Problem: I is blocking because there is no memory update involved.
-- I need to rewrite this section so as to not be based on RAM updates.
-- Problem 2: We don't want to dispatch the same request twice.
step :: CPUState -> RAMStatus -> (CPUState, RAMAction, Maybe Output)
step (CPU state pending Yes) NoUpdate = (CPU state pending Yes, X, Nothing)
step (CPU state pending _  ) update   = (CPU state' pending'' waiting', action, output)
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
    waiting' = case action of
        X -> No
        _ -> Yes

-- If mem response is done, then we want to continue.

cpu :: Signal RAMStatus -> Signal (RAMAction, Maybe Output)
cpu ramstatus = bundle (action, output)
    where
    state  :: Signal CPUState
    state = register (CPU Initializing bootup No) state'
    state' :: Signal CPUState
    action :: Signal RAMAction
    output :: Signal (Maybe Output)
    (state', action, output) = unbundle $ step <$> state <*> ramstatus
