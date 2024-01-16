# Optimizer

## Man page

```sh
$ dune exec owi -- opt --help
OWI-OPT(1)                        Owi Manual                        OWI-OPT(1)

NAME
       owi-opt - Optimize a module

SYNOPSIS
       owi opt [--debug] [--unsafe] [OPTION]… ARG

OPTIONS
       -d, --debug
           debug mode

       -u, --unsafe
           skip typechecking pass

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show  this  help  in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the  format  is  pager  or  plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi opt exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

Owi 11VERSION11                                                     OWI-OPT(1)
```
