# ML to RISC-V Compiler

ML (and C) to RISC-V Compiler written in Ocaml, using LLVM as the IR.<br>This was a project for Compiler Construction course.

## Building from Source

1. Clone the repository:
```bash
git clone https://github.com/cc0ffee/ML2RISCV-compiler.git
cd ML2RISCV-compiler
```

2. Run make:
```bash
make all
```

> NOTE: You might need to run `eval $(opam env)` once you install dependencies

## Usage

### Compilation

To compile an ML or C source file:
```bash
./main [ml-or-c-filepath]
```

### Testing

Within the repository includes two folders c-tests and ml-tests to be used as example compilations. 
Use [Venus Simulator](https://venus.kvakil.me/) or a local environment to run the RISC-V output. The result will be recorded in the `a0` register.
