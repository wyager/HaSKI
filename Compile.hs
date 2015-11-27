module Compile (parseNCompile) where
import Defs (Serialize, serialize, Ptr(..))
import qualified Defs as D
import Memory (Memory, read, write, emptyMem)
import CLaSH.Prelude

parseChar 'S' = S
parseChar 'K' = K
parseChar 'I' = I
parseChar x   = L (toEnum . fromEnum $ x)

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

parseNCompile :: String -> (Memory, Ptr D.SKI)
parseNCompile = compile . parse



--data SKI l = S | K | I | T (SKI l) (SKI l) | Lit l deriving (Eq, Show)

data SKI = S | K | I | T SKI SKI | L (Signed 8) deriving Show

p (Ptr x) = x

compile :: SKI -> (Memory, Ptr D.SKI)
compile term = let (mem, entry) = compile' term emptyMem (Ptr 0) in (mem, pred entry)

compile' :: SKI -> Memory -> Ptr D.SKI -> (Memory, Ptr D.SKI)
compile' term mem base = case term of
    S   -> (write mem base (serialize D.S),     succ base)
    K   -> (write mem base (serialize D.K),     succ base)
    I   -> (write mem base (serialize D.I),     succ base)
    L c -> (write mem base (serialize (D.L c)), succ base)
    T t1 t2 -> (write mem'' base'' (serialize (D.T (pred base') (pred base''))), succ base'')
        where
        (mem', base') = compile' t1 mem base
        (mem'', base'') = compile' t2 mem' base'

main = print $ compile $ parse $ "SSIl"