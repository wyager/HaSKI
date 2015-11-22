data Tok = Term Terminal | Sequence [Tok]

data Terminal = St | Kt | It 

tokenize :: String -> Tok
tokenize [] = error "Invalid sequence"
tokenize "S" = Term St
tokenize "K" = Term Kt
tokenize "I" = Term It
tokenize xs  = tokenize' term xs'
    where
    (term, xs') = takeTerm xs

tokenize' :: 

takeTerm :: 


parse :: String -> SKI l
parse xs = parse'
    where
    (term1, xs') = takeTerm xs
    (term2, xs') = 