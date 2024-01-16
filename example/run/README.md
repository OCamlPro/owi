# Concrete interpreter

## Run a simple program

Given a file `42.wat` with the following content:

<!-- $MDX file=42.wat -->
```wat
(module $quickstart
  (func $f
    i32.const 20
    i32.const 22
    i32.add
    drop
  )
  (start $f)
)
```

Running the interpreter is as simple as:

```sh
$ dune exec owi -- run ./42.wat
```

Nothing is happening, so you can add the `--debug` option to print an execution trace:

```sh
$ dune exec owi -- run ./42.wat --debug
parsing      ...
checking     ...
grouping     ...
assigning    ...
rewriting    ...
typechecking ...
linking      ...
interpreting ...
stack        : [  ]
running instr: call 0
calling func : func f
stack        : [  ]
running instr: i32.const 20
stack        : [ i32.const 20 ]
running instr: i32.const 22
stack        : [ i32.const 22 ; i32.const 20 ]
running instr: i32.add
stack        : [ i32.const 42 ]
running instr: drop
stack        : [  ]
stack        : [  ]
```

## Man page

```sh
$ dune exec owi -- run --help=plain
OWI-RUN(1)                        Owi Manual                        OWI-RUN(1)

NAME
       owi-run - Run the concrete interpreter

SYNOPSIS
       owi run [OPTION]… [ARG]…

OPTIONS
       -d, --debug
           debug mode

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

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
       owi run exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

Owi 11VERSION11                                                     OWI-RUN(1)
```
