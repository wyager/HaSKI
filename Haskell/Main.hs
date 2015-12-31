import Hardware.Sim.Compile (format, encode, compile)
import Hardware.Sim.Harness (evaluate)
import Hardware.Model (Output(..))
import Hardware.CPU (Halt(DoHalt))
import CLaSH.Prelude (sampleN)
import Data.Maybe (catMaybes)
import Prelude

-- Sample up to 20,000 outputs, ignore cycles that don't emit any output,
-- and print the output.
main = do
    let prog = hello4
    let ski = compile prog
    let mem = format $ encode ski
    mapM print $ ski
    putStrLn $ map toChar           -- Turn the output into a char
             $ catMaybes            -- Ignore cycles with no output
             $ map fst              -- Throw away the halt bit
             $ takeWhile notHalting -- Stop when CPU halts
             $ sampleN 20000        -- Sample up to 20,000 cycles
             $ evaluate mem         -- Evaluate the compiled program

hello4 = "SII(SII(hello_world!\n))"

ababs = "SII(S(K(ababababab))(SII))"

double = "SIIa"

swap = "S(K(SI))Kab"

toChar :: Output -> Char
toChar (Output c) = toEnum (fromEnum c)

notHalting :: (Maybe Output, Halt) -> Bool
notHalting (_,DoHalt) = False
notHalting (_,_)      = True
