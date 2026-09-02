# owi


## Subcommands

- [`owi c`](owi-c.md)
- [`owi c++`](owi-c--.md)
- [`owi haskell`](owi-haskell.md)
- [`owi llvm`](owi-llvm.md)
- [`owi rust`](owi-rust.md)
- [`owi tinygo`](owi-tinygo.md)
- [`owi version`](owi-version.md)
- [`owi wasm`](owi-wasm.md)
  - [`owi wasm abs`](owi-wasm-abs.md)
  - [`owi wasm analyze`](owi-wasm-analyze.md)
    - [`owi wasm analyze cfg`](owi-wasm-analyze-cfg.md)
    - [`owi wasm analyze cg`](owi-wasm-analyze-cg.md)
  - [`owi wasm fmt`](owi-wasm-fmt.md)
  - [`owi wasm fuzz`](owi-wasm-fuzz.md)
  - [`owi wasm instrument`](owi-wasm-instrument.md)
    - [`owi wasm instrument label`](owi-wasm-instrument-label.md)
  - [`owi wasm iso`](owi-wasm-iso.md)
  - [`owi wasm of_wat`](owi-wasm-of_wat.md)
  - [`owi wasm replay`](owi-wasm-replay.md)
  - [`owi wasm run`](owi-wasm-run.md)
  - [`owi wasm script`](owi-wasm-script.md)
    - [`owi wasm script abstract`](owi-wasm-script-abstract.md)
    - [`owi wasm script concrete`](owi-wasm-script-concrete.md)
    - [`owi wasm script symbolic`](owi-wasm-script-symbolic.md)
  - [`owi wasm sym`](owi-wasm-sym.md)
  - [`owi wasm to_wat`](owi-wasm-to_wat.md)
  - [`owi wasm validate`](owi-wasm-validate.md)
- [`owi zig`](owi-zig.md)

## Help

```text
NAME
       owi - Seamless program analysis for C, C++, Go, Rust, Wasm and Zig.

SYNOPSIS
       owi [COMMAND] …

COMMANDS
       c [OPTION]… FILE…
           Compile a C file to Wasm and run the symbolic interpreter on it

       c++ [OPTION]… FILE…
           Compile a C++ file to Wasm and run the symbolic interpreter on it

       haskell [OPTION]… FILE…
           Compile a Haskell file to Wasm and run the symbolic interpreter on
           it

       llvm [OPTION]… FILE…
           Compile LLVM IR/bitcode to Wasm and run the symbolic interpreter
           on it

       rust [OPTION]… FILE…
           Compile a Rust file to Wasm and run the symbolic interpreter on it

       tinygo [OPTION]… FILE…
           Compile a TinyGo file to Wasm and run the symbolic interpreter on
           it

       version [OPTION]…
           Print some version informations

       wasm [COMMAND] …
           Work with Wasm programs.

       zig [OPTION]… FILE…
           Compile a Zig file to Wasm and run the symbolic interpreter on it

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.
```
