# Wat2wasm

## Basic example

Given a file `42.wat`:

```sh
$ owi wat2wasm ./42.wat
$ owi run ./42.wasm --debug
typechecking ...
linking      ...
interpreting ...
stack        : [  ]
running instr: call 0
calling func : func anonymous
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
       -d, --debug
           debug mode

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

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
       owi wat2wasm exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
