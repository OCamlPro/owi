[owi](owi.md) › **owi go**

# owi go

## Subcommands

- [`owi go sym`](owi-go-sym.md)

## Help

```text
NAME
       owi-go - Work with Go programs.

SYNOPSIS
       owi go [COMMAND] …

COMMANDS
       sym [OPTION]… FILE…
           Run the symbolic execution engine on a Go program.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi go exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
