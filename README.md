# Agda2Rust

Agda2Rust is an experimental backend for the Agda compiler.

This backend was built for the course Research Project (CSE3000) as part of my bachelors thesis at the TU Delft.

## Configuration

The Agda2Rust backend supports two cli arguments which can also be found when running `agda2rust --help`.

- `--lazy-evaluation`: Insert delay and force operations to enable lazy evaluation
- `--rust-no-main`: Don't emit main function and module declarations
