# `owi wat2wasm`

```sh
$ owi wat2wasm --help=plain
NAME
       owi-wat2wasm - Generate a binary format file (.wasm) from a text
       format file (.wat)

SYNOPSIS
       owi wat2wasm [OPTION]… FILE

ARGUMENTS
       FILE (required)
           source file

OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of auto, always or never.

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -u, --unsafe
           skip typechecking pass

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
       owi wat2wasm exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi wat2wasm:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
