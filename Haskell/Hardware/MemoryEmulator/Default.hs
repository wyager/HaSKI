module Hardware.MemoryEmulator.Default (defaultContents) where

import CLaSH.Prelude

-- The contents of memory.
-- Starts with the compiled SKI program and has zeroes everywhere else.
-- Note that, for purposes of making this project easy to compile to different
-- FPGA families, we are "simulating" RAM in FPGA hardware.
-- This is horrendously inefficient, so we can't have very much "RAM".
-- See Hardware.hs for more details.
defaultContents :: Vec 0x200 (BitVector 64)
defaultContents = hello4 ++ repeat 0

-- These programs are generated using Compile.hs

-- Program: "SII(SII(hello_world!))"
-- This program prints "hello_world!" 4 times.
hello4 = 0x3000000040000006 :>
         0x3000000080000005 :>
         0x30000000c0000004 :>
         0x0000000000000000 :>
         0x2000000000000000 :>
         0x2000000000000000 :>
         0x30000001c000000c :>
         0x300000020000000b :>
         0x300000024000000a :>
         0x0000000000000000 :>
         0x2000000000000000 :>
         0x2000000000000000 :>
         0x3000000340000022 :>
         0x3000000380000021 :>
         0x30000003c0000020 :>
         0x300000040000001f :>
         0x300000044000001e :>
         0x300000048000001d :>
         0x30000004c000001c :>
         0x300000050000001b :>
         0x300000054000001a :>
         0x3000000580000019 :>
         0x30000005c0000018 :>
         0x4000000000000068 :>
         0x4000000000000065 :>
         0x400000000000006c :>
         0x400000000000006c :>
         0x400000000000006f :>
         0x400000000000005f :>
         0x4000000000000077 :>
         0x400000000000006f :>
         0x4000000000000072 :>
         0x400000000000006c :>
         0x4000000000000064 :>
         0x4000000000000021 :>
         Nil

-- Program: "SII(S(K(ababababab))(SII))"
-- This program prints "ababab..." forever.
ababs = 0x3000000040000006 :>
        0x3000000080000005 :>
        0x30000000c0000004 :>
        0x0000000000000000 :>
        0x2000000000000000 :>
        0x2000000000000000 :>
        0x30000001c000001e :>
        0x3000000200000009 :>
        0x0000000000000000 :>
        0x300000028000000b :>
        0x1000000000000000 :>
        0x300000030000001d :>
        0x300000034000001c :>
        0x300000038000001b :>
        0x30000003c000001a :>
        0x3000000400000019 :>
        0x3000000440000018 :>
        0x3000000480000017 :>
        0x30000004c0000016 :>
        0x3000000500000015 :>
        0x4000000000000061 :>
        0x4000000000000062 :>
        0x4000000000000061 :>
        0x4000000000000062 :>
        0x4000000000000061 :>
        0x4000000000000062 :>
        0x4000000000000061 :>
        0x4000000000000062 :>
        0x4000000000000061 :>
        0x4000000000000062 :>
        0x30000007c0000022 :>
        0x3000000800000021 :>
        0x0000000000000000 :>
        0x2000000000000000 :>
        0x2000000000000000 :>
        Nil
