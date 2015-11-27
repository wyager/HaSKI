ask :: State -> (MemRequest, WaitingState)

update :: (MemResponse, WaitingState) -> Either State WaitingState


step :: Either State WaitingState -> MemResponse -> (MemRequest, Either State WaitingState)
-- This should only occur at the beginning
step (Left state) _ = let (req, state') = ask state in (req, Right state')
step (Right state) resp = case (resp, state) of
    ()