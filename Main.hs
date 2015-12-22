import Hardware2.Compile (format, compile)
import Hardware2.Harness (evaluate)
import CLaSH.Prelude (sampleN)

import Prelude

-- Sample up to 200 outputs, ignore cycles that don't emit any output, and print the output.
main = do
    let prog = "SII(SII(hello_world!\n))"
    let ski = compile prog
    let mem = format prog
    mapM print $ ski
    mapM print $ sampleN 200 $ evaluate mem
