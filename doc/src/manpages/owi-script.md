# `owi script`

```sh
$ owi script --help=plain
NAME
       owi-script - Run a reference test suite script

SYNOPSIS
       owi script COMMAND …

COMMANDS
       concrete [--no-exhaustion] [OPTION]… FILE…
           Run a reference test suite script using the concrete interpreter

       symbolic [--no-exhaustion] [OPTION]… FILE…
           Run a reference test suite script using the symbolic interpreter

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi script exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <owi.wildcat119@passmail.com>.

SEE ALSO
       owi(1)

```
