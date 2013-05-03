*Parsing and semantic checking works. LLVM code generation is under progress.
The compiler with working LLVM code generation will be pushed on 4th May.*

# Rascal

Rascal is an LLVM-based compiler written in Rust for a small subset of Pascal
with full debug support.

Currently Rascal works only on Linux x86_64.

Requirements:

* Rust compiler
* Rust bindings for LLVM:
  * Rust module: `$RUST_SRC_DIR/librustc/lib/llvm.rs`
  * LLVM library for Rust: `$RUST_INST_DIR/lib/librustllvm.so`
* clang compiler matching the LLVM version used in Rust

## Building

Steps:

1. Copy `Makefile.in` to `Makefile`.
2. Fill `Makefile` with your path to the directory with the LLVM library for
   Rust (`librustllvm.so`).
3. Copy `rascal.rc.in` to `rascal.rc`.
4. Fill `rascal.rc` with your path to the LLVM Rust bindings module
   (`llvm.rs`).
5. Run `make`.

## Running

Steps:

1. Modify `LD_LIBRARY_PATH` environment variable to contain the directory with
   the LLVM library for Rust (`librustllvm.so`).
2. Run `rascal <source-file>`.

## Building executables with Rascal

Suppose we want to build an executable for `example.pas`.

1. Run `rascal example.pas`. It will produce a file `example.bc` containing
   LLVM bitcode with full debug information.
2. Run `clang -g example.bc`. It will produce an executable file `example`.

## Notes on debugging executables produced by Rascal

1. The main block of a program is compiled as the `main()` function.

## License

Rascal is distributed under the terms of the MIT License. See `LICENSE`
file for details.
