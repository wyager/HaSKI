# Building HaSKI

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

The output goes to `verilog/Hardware.topEntity/haski.v`.

## Simulate

```bash
cd Haskell
clash Main.hs -outputdir build
./Main
```

Compiles the `hello4` program, loads it into simulated RAM, and runs
the evaluator. Should print `hello_world!` four times.
