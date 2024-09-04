# Examples

## Owi binary

- [`owi c`]
- [`owi conc`]
- [`owi fmt`]
- [`owi opt`]
- [`owi run`]
- [`owi script`]
- [`owi sym`]
- [`owi validate`]
- [`owi wasm2wat`]
- [`owi wat2wasm`]

## Owi library

- [OCaml library]
- [import OCaml functions in a Wasm module]

## Man page

```sh
$ owi --help=plain
NAME
       owi - OCaml WebAssembly Interpreter

SYNOPSIS
       owi [COMMAND] …

COMMANDS
       c [OPTION]… [ARG]…
           Compile a C file to Wasm and run the symbolic interpreter on it

       conc [OPTION]… [ARG]…
           Run the concolic interpreter

       fmt [--inplace] [OPTION]… [ARG]…
           Format a .wat or .wast file

       instrument [--debug] [--symbolic] [--unsafe] [OPTION]… [ARG]…
           Generate an instrumented file with runtime assertion checking
           coming from Weasel specifications

       opt [--debug] [--output=FILE] [--unsafe] [OPTION]… ARG
           Optimize a module

       replay [OPTION]… ARG
           Replay a module containing symbols with concrete values in a
           replay file containing a model

       run [OPTION]… [ARG]…
           Run the concrete interpreter

       script [OPTION]… [ARG]…
           Run a reference test suite script

       sym [OPTION]… [ARG]…
           Run the symbolic interpreter

       validate [--debug] [OPTION]… [ARG]…
           Validate a module

       wasm2wat [--emit-file] [--output=FILE] [OPTION]… ARG
           Generate a text format file (.wat) from a binary format file
           (.wasm)

       wat2wasm [OPTION]… ARG
           Generate a binary format file (.wasm) from a text format file
           (.wat)

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
       Email them to <contact@ndrs.fr>.

```

[`owi c`]: ./c
[`owi conc`]: ./conc
[`owi fmt`]: ./fmt
[`owi opt`]: ./opt
[`owi run`]: ./run
[`owi script`]: ./script
[`owi sym`]: ./sym
[`owi validate`]: ./validate
[`owi wasm2wat`]: ./wasm2wat
[`owi wat2wasm`]: ./wat2wasm
[import OCaml functions in a Wasm module]: ./define_host_function
[OCaml library]: ./lib
