# `owi instrument`

```sh
$ owi instrument --help=plain
NAME
       owi-instrument - Instrument a program in various ways

SYNOPSIS
       owi instrument COMMAND …

COMMANDS
       rac [OPTION]… FILE…
           Generate an instrumented file with runtime assertion checking
           coming from Weasel specifications

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi instrument exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```

