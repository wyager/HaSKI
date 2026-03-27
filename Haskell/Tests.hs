{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for the HaSKI SKI-combinator evaluator.
--
-- Tests the binary encoding (binarize/unbinarize roundtrip) and a
-- small hardware simulation. Run with:
--
--   ghc -iHaskell Tests.hs -e main
module Main (main, tests) where

import Clash.Prelude
import qualified Prelude as P
import Control.Exception (try, evaluate, SomeException)
import Clash.XException (XException)

import Hardware.Model (SKI(..), Ptr(..), Output(..),
                       binarize, unbinarize)

-- Test harness ---------------------------------------------------------------

data Result = Pass | Fail String deriving (Eq)

expect :: (Eq a, Show a) => String -> a -> a -> (String, Result)
expect name actual expected
    | actual == expected = (name, Pass)
    | otherwise = (name, Fail msg)
  where
    msg = "expected " P.++ show expected P.++ " but got " P.++ show actual

expectTrue :: String -> Bool -> (String, Result)
expectTrue name b = expect name b True

tests :: [(String, Result)]
tests = roundtripTests

report :: [(String, Result)] -> IO ()
report results = do
    P.mapM_ printLine results
    let failures = [msg | (_, Fail msg) <- results]
    P.putStrLn ""
    P.putStrLn $ show (P.length results - P.length failures)
              P.++ "/" P.++ show (P.length results) P.++ " passed"
    if P.null failures
       then P.putStrLn "ALL TESTS PASSED"
       else P.error "TESTS FAILED"
  where
    printLine (name, Pass)    = P.putStrLn $ "PASS  " P.++ name
    printLine (name, Fail m)  = P.putStrLn $ "FAIL  " P.++ name P.++ ": " P.++ m

main :: IO ()
main = do
    report tests
    invalidTagTests

-- Binarize / unbinarize roundtrip --------------------------------------------
-- Tag encoding (4 bits at [63:60]):
--   0=S 1=K 2=I 3=T 4=L   5-15=invalid

-- SKI has no Eq instance (by design — Ptr equality is reference not
-- structural), so we compare via the binary encoding.
roundtrip :: SKI -> Bool
roundtrip s = binarize (unbinarize (binarize s)) == binarize s

roundtripTests :: [(String, Result)]
roundtripTests =
    [ expectTrue "roundtrip S"  (roundtrip S)
    , expectTrue "roundtrip K"  (roundtrip K)
    , expectTrue "roundtrip I"  (roundtrip I)
    , expectTrue "roundtrip T"  (roundtrip (T (Ptr 123) (Ptr 456)))
    , expectTrue "roundtrip L"  (roundtrip (L (Output 0xDEADBEEF)))
    , -- Direct tag checks on binarize output
      expect "binarize S tag"   (slice d63 d60 (binarize S))                0
    , expect "binarize K tag"   (slice d63 d60 (binarize K))                1
    , expect "binarize I tag"   (slice d63 d60 (binarize I))                2
    , expect "binarize T tag"   (slice d63 d60 (binarize (T (Ptr 0) (Ptr 0)))) 3
    , expect "binarize L tag"   (slice d63 d60 (binarize (L (Output 0))))   4
    ]

-- Invalid-tag tests (must throw, so they run in IO) --------------------------

invalidTagTests :: IO ()
invalidTagTests = do
    check "unbinarize tag 5 throws"  (unbinarize (tagWord 5))
    check "unbinarize tag 15 throws" (unbinarize (tagWord 15))
  where
    tagWord :: BitVector 4 -> BitVector 64
    tagWord t = t ++# (0 :: BitVector 60)
    check name ski = do
        -- force the tag decode; catch the errorX
        r <- try (evaluate (binarize ski))
             :: IO (Either SomeException (BitVector 64))
        case r of
            Left _  -> P.putStrLn $ "PASS  " P.++ name
            Right _ -> P.error $ "FAIL  " P.++ name P.++ ": expected exception"
