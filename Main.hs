import Hardware2.Compile (format)
import Hardware2.Harness (evaluate)
import CLaSH.Prelude (sampleN)

import Prelude

-- Sample up to 200 outputs, ignore cycles that don't emit any output, and print the output.
main = mapM print $ sampleN 200 $ evaluate $ format "SII(SII(hello_world!\n))"
