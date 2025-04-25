# Weasel

Weasel stands for WEbAssembly Specification Language, it can be used to annotate Webassembly text modules in [custom annotation](https://github.com/WebAssembly/annotations). Annotated modules can be instrumented to perform runtime assertion checking thanks to owi's code generator.

Commands and options related to Weasel:
- `owi instrument` to instrument an annotated text module.
- `--rac` for `run`, `sym` and `conc` to instrument an annotated text module and perform runtime assertion checking.
- `--srac` for `sym` and `conc` to instrument an annotated text module and perform runtime assertion checking symbolically.

The formal specification of Weasel can be found in `src/annot/spec.ml`.

## Basic example

Suppose we have a function returning its parameter plus three:

```wast
(module
  (;...;)
  (func $plus_three (param $x i32) (result i32)
    local.get $x
    i32.const 3
    i32.add
  )
  (;...;)
)
```

With Weasel, we can annotate this function by specifying its postconditions:

```wast
(module
  (;...;)
  (@contract $plus_three
    (ensures (= result (+ $x 3)))
  )
  (func $plus_three (param $x i32) (result i32)
    local.get $x
    i32.const 3
    i32.add
  )
  (;...;)
)
```

`owi instrument` generates a new wrapper function checking this postcondition:

```sh
$ owi instrument plus_three.wat
$ cat plus_three.instrumented.wat
(module
  (import "symbolic" "assert" (func $assert  (param i32)))
  (type (sub final  (func (param $x i32) (result i32))))
  (type (sub final  (func)))
  (type (sub final  (func (param i32))))
  (type (sub final  (func (result i32))))
  (func $plus_three (param $x i32) (result i32)
    local.get 0
    i32.const 3
    i32.add
  )
  (func $start
    i32.const 42
    call 3
    drop
  )
  (func $__weasel_plus_three (param $x i32) (result i32) (local $__weasel_temp i32) (local $__weasel_res_0 i32)
    local.get 0
    call 1
    local.set 2
    local.get 2
    local.get 0
    i32.const 3
    i32.add
    i32.eq
    call 0
    local.get 2
  )
  (start 2)
)
```

We can perform runtime assertion checking either by `owi sym plus_three.instrumented.wat` or by `owi sym --rac plus_three.wat`.

```sh
$ owi sym plus_three.instrumented.wat
All OK!
$ owi sym --rac plus_three.wat
All OK!
```

## Man page

```sh
$ owi instrument --help=plain
NAME
       owi-instrument - Generate an instrumented file with runtime assertion
       checking coming from Weasel specifications

SYNOPSIS
       owi instrument [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of auto, always or never.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       --symbolic
           generate instrumented module that depends on symbolic execution

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
       owi instrument exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi instrument:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
