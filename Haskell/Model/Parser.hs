module Model.Parser (parse) where

import Model.Model (SKI(S,K,I,T,L))

-- Parser from https://github.com/catseye/Dipple/blob/master/haskell/SKI.hs
-- Thanks!
parseChar 'S' = S
parseChar 'K' = K
parseChar 'I' = I
parseChar x   = L x

kParse (' ':rest) =
    kParse rest
kParse ('(':rest) =
    let
        (t, rest') = kParse rest
    in
        bParse rest' t
kParse (char:rest) =
    bParse rest (parseChar char)

bParse [] acc =
    (acc, [])
bParse (' ':rest) acc =
    bParse rest acc
bParse (')':rest) acc =
    (acc, rest)
bParse ('(':rest) acc =
    let
        (t, rest') = kParse rest
    in
        bParse rest' (T acc t)
bParse (char:rest) acc =
    bParse rest $ T acc (parseChar char)


parse = fst . kParse