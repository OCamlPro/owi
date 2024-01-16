# Formatter

## Man page

```sh
$ dune exec owi -- fmt --help
OWI-FMT(1)                        Owi Manual                        OWI-FMT(1)

NAME
       owi-fmt - Format a .wat or .wast file

SYNOPSIS
       owi fmt [--inplace] [OPTION]… ARG

OPTIONS
       -i, --inplace
           Format in-place, overwriting input file

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show  this  help  in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the  format  is  pager  or  plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi fmt exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

Owi 11VERSION11                                                     OWI-FMT(1)
```
