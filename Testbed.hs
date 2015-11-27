import HaSKI2 (run)
import Compile (parseNCompile)
import CLaSH.Prelude

main = mapM print $ sampleN 4 $ uncurry run $ parseNCompile "SSIa"