[owi](owi.md) › [owi wasm](owi-wasm.md) › **owi wasm script**

# owi wasm script

## Subcommands

- [`owi wasm script abstract`](owi-wasm-script-abstract.md)
- [`owi wasm script concrete`](owi-wasm-script-concrete.md)
- [`owi wasm script symbolic`](owi-wasm-script-symbolic.md)

## Help

```text
NAME
       owi-wasm-script - Run a reference test suite script (.wast).

SYNOPSIS
       owi wasm script COMMAND …

COMMANDS
       abstract [--no-exhaustion] [OPTION]… FILE…
           Run a reference test suite (.wast) using the abstract interpreter.

       concrete [--no-exhaustion] [OPTION]… FILE…
           Run a reference test suite (.wast) using the concrete interpreter.

       symbolic [--no-exhaustion] [OPTION]… FILE…
           Run a reference test suite (.wast) using the symbolic interpreter.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi wasm script exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
