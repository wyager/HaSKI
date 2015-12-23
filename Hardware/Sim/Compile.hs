module Hardware.Sim.Compile (compile, format) where

import Prelude

import Hardware.Model (SKI(S,K,I,T,L), Ptr(Ptr), Output(Output), binarize)

import Hardware.Sim.Memory (Memory, fromStack)

-- Representation without explicit pointers
data SKI' = S' | K' | I' | T' SKI' SKI' | L' Output deriving Show

-- Parser from https://github.com/catseye/Dipple/blob/master/haskell/SKI.hs
-- Thanks!
parseChar 'S' = S'
parseChar 'K' = K'
parseChar 'I' = I'
parseChar x   = L' . Output . toEnum . fromEnum $ x

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
        bParse rest' (T' acc t)
bParse (char:rest) acc =
    bParse rest $ T' acc (parseChar char)

parse = fst . kParse

-- Takes the pointer-free Haskell representation and turns it into
-- a representation with explicit pointers. The returned list corresponds
-- to the memory starting at 0.
linearize :: SKI' -> [SKI]
linearize ski' = (\(stack,base,entry) -> stack) $ linearize' 0 [] ski'

linearize' :: Ptr -> [SKI] -> SKI' -> ([SKI], Ptr, Ptr)
linearize' base stack next = (stack', base', entry)
    where
    (stack', base', entry) = case next of
        S' ->   (S : stack ,   base + 1, base)
        K' ->   (K : stack ,   base + 1, base)
        I' ->   (I : stack ,   base + 1, base)
        L' c -> ((L c):stack , base + 1, base)
        T' l' r' -> (rstack, rbase, tentry)
            where
            (tstack, tbase, tentry) = (T lentry rentry : stack, base + 1, base)
            (lstack, lbase, lentry) = linearize' tbase tstack l'
            (rstack, rbase, rentry) = linearize' lbase lstack r'

compile :: String -> [SKI]
compile = reverse . linearize . parse

format :: String -> Memory
format =  fromStack . map binarize . compile
