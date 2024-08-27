# Wasm2wat

## Basic example

Given a file `42.wasm`:

```sh
$ owi wasm2wat ./42.wasm
(module
  (type (sub final  (func)))
  (func
    i32.const 20
    i32.const 22
    i32.add
    drop
  )
  (start 0)
)
```

## Man page

```sh
$ owi wasm2wat --help=plain
NAME
       owi-wasm2wat - Generate a text format file (.wat) from a binary format
       file (.wasm)

SYNOPSIS
       owi wasm2wat [--emit-file] [--output=FILE] [OPTION]… ARG

OPTIONS
       --emit-file
           Emit (.wat) files from corresponding (.wasm) files.

       -o FILE, --output=FILE
           Write output to a file.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi wasm2wat exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
