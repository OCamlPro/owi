# `owi analyze`

```sh
$ owi analyze --help=plain
NAME
       owi-analyze - Analyze a program in different possible ways

SYNOPSIS
       owi analyze COMMAND …

COMMANDS
       cfg [--entry-point=FUNCTION] [OPTION]… FILE
           Build a Control-Flow Graph

       cg [--call-graph-mode=VAL] [--entry-point=FUNCTION] [OPTION]… FILE
           Build a call graph

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi analyze exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
