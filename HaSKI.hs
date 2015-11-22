import Data.Magma (Magma, (<>))

-- Parser from https://github.com/catseye/Dipple/blob/master/haskell/SKI.hs
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




--data SKI l = S | K | I | T (SKI l) (SKI l) | Lit l deriving (Eq, Show)

data SKI = S | K | I | T SKI SKI | L Char deriving Show

-- Stack, state, output
-- Note: output is kept in reverse order until we hit Terminal
data State = State [SKI] SKI | Terminal deriving Show

data Output = C Char | Empty deriving Show

step :: State -> (State, Output)
step (State stack         (T a b)) = (State (b : stack)           a, Empty)
step (State (s:stack)     I      ) = (State stack                 s, Empty)
step (State (x:y:stack)   K      ) = (State stack                 x, Empty)
step (State (x:y:z:stack) S      ) = (State (z : (T y z) : stack) x, Empty)
step (State (s:stack)     (L l)  ) = (State stack                 s, C l)
step (State []            (L l)  ) = (Terminal                     , C l)

terminal :: State -> Bool
terminal Terminal = True
terminal _        = False

initial :: SKI -> State
initial ski = State [] ski

takeUntil f [] = []
takeUntil f (x:xs) = if f x then [x] else x : takeUntil f xs

main = print $ [char | (state, C char) <- steps]

steps = takeUntil (terminal . fst) $ iterate (\(state,out) -> step state) $ (initial $ parse "SII(S(K(ab))(SII))", Empty)

(#) = flip ($)

--evaluate :: Magma l => SKI l -> SKI l
--evaluate (Lit l) = (Lit l)
--evaluate S = S
--evaluate K = K
--evaluate I = I
--evaluate (T I x) = x
--evaluate (T (T K x) y) = x
--evaluate (T (T (T S x) y) z) = T (T x z) (T y z)
--evaluate (T (Lit a) (Lit b)) = Lit (a <> b)
--evaluate (T a b) = T (evaluate a) (evaluate b)


--instance Magma [a] where
--    (<>) = (++)

--evaluate' :: (Eq l, Magma l) => SKI l -> SKI l
--evaluate' term 
--    | term' == term = term
--    | otherwise     = evaluate' term'
--    where term' = evaluate term


--main = print $ evaluate' $ flipIt (Lit "a") (Lit "b")

--double = (T (T (T S I) I) (Lit "a"))

--flipIt a b = (T (T (T (T S (T K (T S I))) K) a) b)


---------------------------------------------------------------


-- data = 

-- data SKI t = S (SKI t) (SKI t) (SKI t) | K (SKI t) (SKI t) | I (SKI t) | Lit t deriving (Show)

--evaluate :: Magma t => SKI t -> t
--evaluate (I s) = evaluate s
--evaluate (K a b) = evaluate a
--evaluate (S a b c) = (evaluate a <> evaluate c) <> (evaluate b <> evaluate c)
--evaluate (Lit t) = t

--data Fun a = Fun (a -> Fun a) | Const a

--instance Show a => Show (Fun a) where
--    show (Fun _) = "unevaluated function"
--    show (Const a) = show a

--instance Magma (Fun a) where
--    (Fun f) <> (Const a) = f a
--    (Const a) <> (Fun f) = f a
--    f <> (Fun g) = Fun (\x -> f <> g x)
--    (Const a) <> (Const b) = Const b

--lift :: (a -> a) -> (a -> Fun a)
--lift f = Const . f


--main = print $ Lit (Const 0)