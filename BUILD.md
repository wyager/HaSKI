# Building HaSKI

See `README.md` for what HaSKI actually does (SKI combinator evaluator).

## Requirements

- GHC 9.0–9.6 (tested with 9.4.7)
- `clash-ghc` 1.8.2
- `clash-prelude` 1.8.2 (must match `clash-ghc` exactly — Clash has a hard
  ABI check)
- The three GHC typechecker plugins Clash uses: `ghc-typelits-knownnat`,
  `ghc-typelits-natnormalise`, `ghc-typelits-extra`

Install via cabal:

```bash
cabal update
cabal install clash-ghc-1.8.2 --installdir=$HOME/.local/bin --install-method=copy
cabal install --lib clash-prelude-1.8.2 \
    ghc-typelits-knownnat ghc-typelits-natnormalise ghc-typelits-extra \
    --constraint="clash-prelude==1.8.2"
```

## Generate HDL

```bash
cd Haskell
clash --verilog Hardware.hs -outputdir build
```

The output goes to `verilog/Hardware.topEntity/haski.v`.

## Simulate

```bash
cd Haskell
clash Main.hs -outputdir build
./Main
```

Compiles the `hello4` program, loads it into simulated RAM, and runs
the evaluator. Should print `hello_world!` four times.
