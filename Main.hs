import Hardware.Sim.Compile (format, compile)
import Hardware.Sim.Harness (evaluate)
import Hardware.Model (Output(..))
import CLaSH.Prelude (sampleN)
import Data.Maybe (catMaybes)
import Prelude

-- Sample up to 200 outputs, ignore cycles that don't emit any output, and print the output.
main = do
    let prog = hello4
    let ski = compile prog
    let mem = format prog
    mapM print $ ski
    putStrLn $ map toChar $ catMaybes $ sampleN 2000 $ evaluate mem

hello4 = "SII(SII(hello_world!\n))"

ababs = "SII(S(K(ab))(SII))"

double = "SIIa"

swap = "S(K(SI))Kab"

toChar :: Output -> Char
toChar (Output c) = toEnum (fromEnum c)
