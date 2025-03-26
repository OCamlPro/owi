# Validator

## Basic example

Given a file `type_error.wat` with the following content:

<!-- $MDX file=type_error.wat -->
```wat
(module $quickstart
  (func $f
    i32.const 20
    i32.const 22
    i32.add
    i32.add
    drop
  )
  (start $f)
)
```

Running the validator is as simple as:

```sh
$ owi validate ./type_error.wat
type mismatch (expected [i32 i32] but stack is [i32])
[35]
```

You can also print a more detailed trace with the `--debug` option:

```sh
$ owi validate ./type_error.wat --debug
parsing      ...
checking     ...
grouping     ...
assigning    ...
rewriting    ...
typechecking ...
type mismatch (expected [i32 i32] but stack is [i32])
[35]
```

## Man page

```sh
$ owi validate --help=plain
NAME
       owi-validate - Validate a module

SYNOPSIS
       owi validate [--debug] [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       -d, --debug
           debug mode

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi validate exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
