# Symbolic interpreter

## Basic example

First, you need to perform a symbolic run and to store the output model in a file. Given the following `mini.wat` file containing symbols:

<!-- $MDX file=mini.wat -->
```wat
(module
  (import "symbolic" "i32_symbol" (func $gen_i32 (result i32)))

  (func $start (local $x i32)
    (local.set $x (call $gen_i32))
    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    )))

  (start $start)
)
```

You can get a model like this:

```sh
$ owi sym ./mini.wat > mini.scfg
owi: [ERROR] Trap: unreachable
owi: [ERROR] Reached problem!
[13]
```

Then you can replay the module execution with the values in the model like this:

```sh
$ owi replay --replay-file mini.scfg mini.wat
owi: [ERROR] unreachable
[94]
```

## Man page

```sh
$ owi replay --help=plain
NAME
       owi-replay - Replay a module containing symbols with concrete values
       in a replay file containing a model

SYNOPSIS
       owi replay [OPTION]… FILE

ARGUMENTS
       FILE (required)
           source file

OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of auto, always or never.

       --entry-point=FUNCTION
           entry point of the executable

       --invoke-with-symbols
           Invoke the entry point of the program with symbolic values instead
           of dummy constants.

       --optimize
           optimize mode

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       --replay-file=FILE (required)
           Which replay file to use

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
       owi replay exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi replay:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
