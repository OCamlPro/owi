# Symbolic interpreter

## Man page

```sh
$ dune exec owi -- sym --help
OWI-SYM(1)                        Owi Manual                        OWI-SYM(1)

NAME
       owi-sym - Run the symbolic interpreter

SYNOPSIS
       owi sym [OPTION]… [ARG]…

OPTIONS
       -d, --debug
           debug mode

       --no-stop-at-failure
           do not stop when a program failure is encountered

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

       -u, --unsafe
           skip typechecking pass

       -w VAL, --workers=VAL (absent=4)
           number of workers for symbolic execution

       --workspace=VAL (absent=owi-out)
           path to the workspace directory

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show  this  help  in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the  format  is  pager  or  plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi sym exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

Owi 11VERSION11                                                     OWI-SYM(1)
```
