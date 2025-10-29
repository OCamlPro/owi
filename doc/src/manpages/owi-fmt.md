# `owi fmt`

```sh
$ owi fmt --help=plain
NAME
       owi-fmt - Format a .wat or .wast file

SYNOPSIS
       owi fmt [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --bench
           enable benchmarks

       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of auto, always or never.

       -i, --inplace
           Format in-place, overwriting input file

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning or OWI_VERBOSITY env)
           Be more or less verbose. LEVEL must be one of quiet, error,
           warning, info or debug. Takes over -v.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi fmt exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi fmt:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```

