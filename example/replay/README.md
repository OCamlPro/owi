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
$ owi sym ./mini.wat > mini.model
Reached problem!
[13]
```

Then you can replay the module execution with the values in the model like this:

```sh
$ owi replay --replay-file mini.model mini.wat
invalid model: can not find the directive `model` while parsing the scfg config
[78]
```

## Man page

```sh
$ owi replay --help=plain
NAME
       owi-replay - Replay a module containing symbols with concrete values
       in a replay file containing a model

SYNOPSIS
       owi replay [OPTION]… ARG

OPTIONS
       -d, --debug
           debug mode

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

       --replay-file=VAL (required)
           Which replay file to use

       -u, --unsafe
           skip typechecking pass

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

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
