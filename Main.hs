import Hardware2.Compile (format, compile)
import Hardware2.Model (Output(..))
import Hardware2.Harness (evaluate)
import CLaSH.Prelude (sampleN)
import Data.Maybe (catMaybes)
import Prelude

-- Sample up to 200 outputs, ignore cycles that don't emit any output, and print the output.
main = do
    let prog = swap
    let ski = compile prog
    let mem = format prog
    mapM print $ ski
    putStrLn $ map (\(Output c) -> (toEnum . fromEnum $ c) :: Char) $ catMaybes $ sampleN 2000 $ evaluate mem

hello4 = "SII(SII(hello_world!\n))"

ababs = "SII(S(K(ab))(SII))"

double = "SIIa"

swap = "S(K(SI))Kab"
