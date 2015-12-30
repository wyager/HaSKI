module Main where

import Prelude
import CLaSH.Prelude (BitVector)
import Hardware.Sim.Compile (compile, encode)
import Text.Printf (printf)
import Data.List (intercalate)

main = do
    putStrLn "Enter program:"
    string <- getLine
    let program = compile string
    putStrLn "Program:"
    mapM_ print program
    let memory = encode program
    let hex = intercalate "\n" (map hexify memory) ++ "\n"
    putStrLn "Writing to mem.hex"
    writeFile "mem.hex" hex

hexify :: BitVector 64 -> String
hexify bv = printf "%016x" (toInteger bv)
