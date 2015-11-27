module Model.StackMachine (evaluate) where

import Model.Model (SKI(S,K,I,T,L))

data State = State {stack :: [SKI], current :: SKI} | Terminal deriving Show

step :: State -> (State, Maybe Char)
step (State stack         (T a b)) = (State (b : stack)           a, Nothing)
step (State (s:stack)     I      ) = (State stack                 s, Nothing)
step (State (x:y:stack)   K      ) = (State stack                 x, Nothing)
step (State (x:y:z:stack) S      ) = (State (z : (T y z) : stack) x, Nothing)
step (State (s:stack)     (L l)  ) = (State stack                 s, Just l)
step (State []            (L l)  ) = (Terminal                     , Just l)

terminal :: State -> Bool
terminal Terminal = True
terminal _        = False

initial :: SKI -> State
initial ski = State [] ski

takeUntil f [] = []
takeUntil f (x:xs) = if f x then [x] else x : takeUntil f xs

steps :: SKI -> [(State, Maybe Char)]
steps program = takeUntil (terminal . fst) $ iterate (\(state,out) -> step state) $ (initial program, Nothing)

evaluate :: SKI -> [Char]
evaluate program = [char | (state, Just char) <- steps program]



