[owi](owi.md) › **owi rust**

# owi rust

## Subcommands

- [`owi rust abs`](owi-rust-abs.md)
- [`owi rust fuzz`](owi-rust-fuzz.md)
- [`owi rust hunt`](owi-rust-hunt.md)
- [`owi rust run`](owi-rust-run.md)
- [`owi rust sym`](owi-rust-sym.md)

## Help

```text
NAME
       owi-rust - Work with Rust programs.

SYNOPSIS
       owi rust [COMMAND] …

COMMANDS
       abs [OPTION]… FILE…
           Run the abstract interpreter.

       fuzz [OPTION]… FILE…
           Run the fuzzer.

       hunt [OPTION]… FILE…
           Hunt bugs by combining the fuzzer and the symbolic execution
           engine.

       run [OPTION]… FILE…
           Run the concrete interpreter.

       sym [OPTION]… FILE…
           Run the symbolic execution engine on a Rust program.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi rust exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
