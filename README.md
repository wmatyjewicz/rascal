# Rascal

Rascal is an LLVM-based compiler written in Rust for a small subset of Pascal
with full debug support.

Rascal has been written with focus on learning Rust and generation of debug
information with LLVM. So far, little effort has been spent to make it work
on multiple platforms, be easy to build or be easy to use.

Currently Rascal works only on Linux x86_64.

Requirements:

* Rust built from the HEAD of the git repository, in particular:
  * the compiler: `rustc`
  * LLVM bindings module: `$RUST_DIR/src/librustc/lib/llvm.rs`
  * LLVM library for Rust:
    `$RUST_DIR/rustllvm/x86_64-unknown-linux-gnu/librustllvm.so`
  * LLVM target code generation tool `llc` that is built together
    with Rust: `$RUST_DIR/llvm/x86_64-unknown-linux-gnu/$BUILD_TYPE/llc`
    (where `$BUILD_TYPE` is the type of LLVM build process,
    eg. `Release+Asserts`)

## Building

Steps:

1. Copy `Makefile.in` to `Makefile`.
2. Fill `Makefile` with your path to the directory with the LLVM library for
   Rust (`librustllvm.so`).
3. Copy `rascal.rc.in` to `rascal.rc`.
4. Fill `rascal.rc` with your path to the LLVM bindings module (`llvm.rs`).
5. Run `make`.

## Running

Steps:

1. Modify `LD_LIBRARY_PATH` environment variable to contain the directory with
   the LLVM library for Rust (`librustllvm.so`).
2. Run `rascal <file>.pas`. It will produce `<file>.bc` containing
   LLVM bitcode with full debug information.
3. Run `llc <file>.bc`. It will produce `<file>.s` with the assembly code.
4. Run `gcc <file>.s` (alternatively clang may be used) to generate

## Notes on debugging executables produced by Rascal

1. All identifiers are lower cased (Pacal is case-insensitive).
2. The main block of a program is compiled as the `main()` function.

## License

Rascal is distributed under the terms of the MIT License. See `LICENSE`
file for details.
