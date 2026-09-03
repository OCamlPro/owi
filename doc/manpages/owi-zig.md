[owi](owi.md) › **owi zig**

# owi zig

## Subcommands

- [`owi zig sym`](owi-zig-sym.md)

## Help

```text
NAME
       owi-zig - Work with Zig programs.

SYNOPSIS
       owi zig [COMMAND] …

COMMANDS
       sym [OPTION]… FILE…
           Run the symbolic execution engine on a Zig program.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi zig exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
