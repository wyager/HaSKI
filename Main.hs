import Hardware.Programs (evaluate, double, swap, ababs, compile)
import CLaSH.Prelude (sampleN)

import Prelude

-- Sample up to 200 outputs, ignore cycles that don't emit any output, and print the output.
main = putStrLn $ map (\(Just c) -> c) $ filter (/= Nothing) $ sampleN 200 $ evaluate $ compile "SII(SII(hello_world!\n))"
