[owi](owi.md) › [owi wasm](owi-wasm.md) › **owi wasm inspect**

# owi wasm inspect

## Subcommands

- [`owi wasm inspect cfg`](owi-wasm-inspect-cfg.md)
- [`owi wasm inspect cg`](owi-wasm-inspect-cg.md)

## Help

```text
NAME
       owi-wasm-inspect - Visualize and get statistics.

SYNOPSIS
       owi wasm inspect [COMMAND] …

COMMANDS
       cfg [--entry-point=FUNCTION] [OPTION]… FILE
           Build a control-flow graph.

       cg [--call-graph-mode=VALUE] [--entry-point=FUNCTION] [OPTION]… FILE
           Build a call graph.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi wasm inspect exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)
```
