# Building HaSKI with Modern Clash

HaSKI was originally written for CλaSH 0.x. It has been ported to
**Clash 1.8.2** with GHC 9.4.

See `README.md` for what HaSKI actually does (SKI combinator evaluator).

## Requirements

Same as any Clash 1.8 project — see Lambda16/BUILD.md for the cabal
install recipe, or use the `fmax-hdl` Docker image:

```bash
docker run --rm -v $PWD:/src -w /src/Haskell \
    us-east1-docker.pkg.dev/gcp-taiga/envfactory/fmax-hdl:v6 \
    clash --verilog Hardware.hs
```

## Generate HDL

```bash
cd Haskell
clash --verilog Hardware.hs -outputdir build
```

The output `verilog/Hardware.topEntity/haski.v` replaces the old
`Hardware_topEntity.v` in `HDL/`. The generated module is now named
`haski` (was `Hardware_topEntity`) per the `Synthesize` annotation.

## Simulate

```bash
cd Haskell
clash Main.hs -outputdir build
./Main
```

Compiles the `hello4` program, loads it into simulated RAM, and runs
the evaluator. Should print `hello_world!` four times.

## What changed from CλaSH 0.x

- **Module rename**: `CLaSH.Prelude` → `Clash.Prelude`.
- **Clock domains**: `Signal a` → `Signal System a`. `cpu`, `ram`,
  `cpuHardware`, `ramHardware`, `evaluate`, `memulate` all gained
  `HiddenClockResetEnable System` constraints.
- **NFDataX**: Every type that ends up inside the CPU register
  (`CPUState`, `State`, `Stack`, `Heap`, `SKIs`, `Pending`, `SKI`,
  `Ptr`, `Output`, `Halt`, `Waiting`, all the `Defs.hs` types) now
  derives `Generic` + `NFDataX`. The sim-only `Memory` (wraps
  `Data.Map`) has a no-op manual `NFDataX` instance.
- **blockRam API**: Old Clash took 5 args (`rdAddr, wrAddr, wrEn,
  wrData`); modern Clash takes 3 (`rdAddr, Maybe (addr, data)`).
  `Hardware/MemoryEmulator/RAM.hs` was rewritten to pack the write
  signals into a `Maybe`.
- **topEntity**: Now `Clock System -> Reset System -> ...` with a
  `Synthesize` annotation. The core loop is factored into a separate
  `haski` function so `withClockResetEnable` scopes correctly.
- **BitVector literals**: The `++#` concatenations in `binarize` now
  have explicit width annotations (`0 :: BitVector 60`) — modern
  Clash is stricter about inferring widths of numeric literals.
- **Pattern exhaustiveness**: `ramstatus` gained a wildcard arm since
  `Bit` is no longer provably exhaustive on `{0,1}`.

The `Model/` directory is pure Haskell (no Clash) and didn't change.
