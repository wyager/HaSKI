module Memory (Memory, read, write, emptyMem) where
import Defs (Ptr(..))
import CLaSH.Prelude hiding (read)
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Text.Printf (printf)

data Memory = Memory (M.Map (Unsigned 30) (BitVector 8))

instance Show Memory where
    show (Memory map) = intercalate "\n" [printf "0x%x: 0x%x" (fromEnum addr) (fromEnum byte) | (addr,byte) <- M.toList map]

emptyMem :: Memory
emptyMem = Memory M.empty

read :: Memory -> Ptr a -> BitVector 64
read (Memory map) (Ptr ptr) 
    | ptr `M.notMember` map = error $ printf "Invalid read ptr :%s" (show ptr)
    | otherwise = (map M.! ptr + 0) ++#
                  (map M.! ptr + 1) ++#
                  (map M.! ptr + 2) ++#
                  (map M.! ptr + 3) ++#
                  (map M.! ptr + 4) ++#
                  (map M.! ptr + 5) ++#
                  (map M.! ptr + 6) ++#
                  (map M.! ptr + 7)

write :: Memory -> Ptr a -> BitVector 64 -> Memory
write (Memory map) (Ptr ptr) val = Memory $ 
                             M.insert (ptr + 0) (slice d63 d56 val) . 
                             M.insert (ptr + 1) (slice d55 d48 val) . 
                             M.insert (ptr + 2) (slice d47 d40 val) . 
                             M.insert (ptr + 3) (slice d39 d32 val) . 
                             M.insert (ptr + 4) (slice d31 d24 val) . 
                             M.insert (ptr + 5) (slice d23 d16 val) . 
                             M.insert (ptr + 6) (slice d15 d8  val) . 
                             M.insert (ptr + 7) (slice d7  d0  val) $ map