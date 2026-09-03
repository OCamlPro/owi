[owi](owi.md) › **owi llvm**

# owi llvm

## Subcommands

- [`owi llvm sym`](owi-llvm-sym.md)

## Help

```text
NAME
       owi-llvm - Work with LLVM programs.

SYNOPSIS
       owi llvm [COMMAND] …

COMMANDS
       sym [OPTION]… FILE…
           Run the symbolic execution engine on a LLVM program.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi llvm exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
