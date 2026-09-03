# owi


## Subcommands

- [`owi c`](owi-c.md)
  - [`owi c sym`](owi-c-sym.md)
- [`owi c++`](owi-c--.md)
  - [`owi c++ sym`](owi-c---sym.md)
- [`owi go`](owi-go.md)
  - [`owi go sym`](owi-go-sym.md)
- [`owi haskell`](owi-haskell.md)
  - [`owi haskell sym`](owi-haskell-sym.md)
- [`owi llvm`](owi-llvm.md)
  - [`owi llvm sym`](owi-llvm-sym.md)
- [`owi rust`](owi-rust.md)
  - [`owi rust sym`](owi-rust-sym.md)
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
  - [`owi zig sym`](owi-zig-sym.md)

## Help

```text
NAME
       owi - Seamless program analysis for C, C++, Go, Haskell, LLVM, Rust,
       Wasm and Zig.

SYNOPSIS
       owi [COMMAND] …

COMMANDS
       c [COMMAND] …
           Work with C programs.

       c++ [COMMAND] …
           Work with C++ programs.

       go [COMMAND] …
           Work with Go programs.

       haskell [COMMAND] …
           Work with Haskell programs.

       llvm [COMMAND] …
           Work with LLVM programs.

       rust [COMMAND] …
           Work with Rust programs.

       version [OPTION]…
           Print some version informations.

       wasm [COMMAND] …
           Work with Wasm programs.

       zig [COMMAND] …
           Work with Zig programs.

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
