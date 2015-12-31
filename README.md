# HaSKI

### An FPGA-based SKI calculus evaluator written in Cλash/Haskell

HaSKI is my attempt at building a reasonably simple hardware-based evaluator
for the dead-simple, turing-complete [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus).

Because [I don't like most HDLs](http://yager.io/talks/CLaSH.pdf), I
decided to do this project using a Haskell-to-hardware compiler called
[Cλash](https://github.com/clash-lang/clash-compiler).

The SKI calculus is defined by three terms (`S`, `K`, and `I`), but these terms
don't have any side effects, so it's hard to make a pure SKI program
do anything interesting. Therefore, I added an `L` (for "Literal") term,
which is just like `I` except that it causes the evaluator to emit a 32-bit
value when evaluated.

For demonstration purposes, I use these 32-bit values to encode characters,
so that the evaluator can print things.

## Directory:

* `Haskell/` contains Haskell/Cλash code.
  * `Compile.hs` compiles SKI programs to binary code (as `.hex` files).
  * `Hardware.hs` contains `topEntity`, which is the final generated circuit.
  * `Main.hs` contains some utilities for simulating the CPU. Mostly used for debugging.
  * `Model/` contains a pure Haskell "demo" evaluator. It's very simple.
    * `Model.hs` contains SKI term definitions.
    * `Parser.hs` contains a simple parser.
    * `Programs.hs` contains example programs.
    * `StackMachine.hs` contains the evaluator.
  * `Hardware/` contains the Cλash code that can actually target hardware.
    * `CPU.hs` ties together memory and evaluator logic.
    * `Defs.hs` contains some useful type definitions.
    * `MMU.hs` contains memory logic.
    * `Model.hs` contains type definitions for basic building blocks (SKI terms and pointers).
    * `StackMachine.hs` contains the core evaluator logic.
    * `MemoryEmulator/` contains some RAM simulation logic.
      * `RAM.hs` contains a fake RAM device implemented in FPGA logic. This makes it easy to target arbitrary FPGA boards, at the expense of RAM size.
      * `Default.hs` contains default pseudo-RAM contents for a few programs.
    * `Sim/` contains evaluator simulation logic. This simulates at a higher level than the hardware simulation, so is easier for debugging.
      * `Compile.hs` contains code for compiling SKI programs (as strings) into various forms (including HaSKI machine code).
      * `Harness.hs` contains code for running the evaluator on programs.
      * `Memory.hs` contains code for simulating a memory device.


## The model evaluator

Because it's many times simpler, I've included a Haskell (not Cλash) SKI
calculus evaluator that isn't compilable to hardware. It's in `Haskell/Model/`. To use
it, run

```
ghci Model/Programs.hs
evaluate hello4
evaluate $ parse "K(Ia)b"
```

I suggest you read this first to understand what the evaluator is doing.

## The hardware evaluator

The hardware evaluator does the same thing as the model evaluator, but
it can be implemented in hardware. This means that the only RAM
is that which is manually controlled over a data bus, which means we
have no stack, no heap, no garbage collection, etc. Therefore, we have to
be a lot more explicit about all the evaluator's possible states,
which means the code gets a lot more complicated.

### Simulation

Using the code in `Hardware/Sim/`, we can simulate the hardware by compiling
it to a binary. That's one of the great things about Cλash; it's all just
Haskell (plus some extensions), so we can compile it to a program just
like normal Haskell code.

To generate and run a binary,

```
clash Main.hs
./Main
```

Looking at the contents of `Main.hs`, you can see that it does the following:

* Compile the program "hello4" (which emits `hello_world!` four times)
* Compiles and formats the program into memory
* Prints the compiled program
* Runs the evaluator with the memory containing the compiled "hello4"
* Prints the character representation of all outputs

### Hardware

This is in progress! Ideally, this would be pretty much done,
but hardware is never as simple as you hope.
