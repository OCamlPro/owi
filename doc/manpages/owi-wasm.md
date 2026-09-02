[owi](owi.md) › **owi wasm**

# owi wasm

## Subcommands

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

## Help

```text
NAME
       owi-wasm - Work with Wasm programs.

SYNOPSIS
       owi wasm [COMMAND] …

COMMANDS
       abs [--entry-point=FUNCTION] [--unsafe] [OPTION]… FILE
           Run the abstract interpreter.

       analyze COMMAND …
           Visualize and get statistics.

       fmt [--inplace] [OPTION]… FILE…
           Format a .wat or .wast file.

       fuzz [OPTION]… FILE
           Run the fuzzer.

       instrument COMMAND …
           Instrument a program in various ways.

       iso [OPTION]… FILE…
           Check the iso-functionnality of two modules by comparing the
           output when calling their exports.

       of_wat [--output=FILE] [--unsafe] [OPTION]… FILE
           Generate a binary file (.wasm) from a text file (.wat).

       replay [OPTION]… FILE
           Replay a module by replacing symbols with concrete values from a
           model.

       run [--timeout=S] [--timeout-instr=I] [--unsafe] [OPTION]… FILE
           Run the concrete interpreter.

       script COMMAND …
           Run a reference test suite script (.wast).

       sym [OPTION]… FILE
           Run the symbolic execution engine.

       to_wat [--emit-file] [--output=FILE] [OPTION]… FILE
           Generate a text file (.wat) from a binary file (.wasm).

       validate [OPTION]… FILE…
           Validate a module.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi wasm exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
