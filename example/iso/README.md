# Checking iso-behaviour of Wasm programs

The `owi iso` command takes two Wasm modules as input.
Then, for every common exports between these two modules, Owi will check their equivalence.

## Simple example

Given the following `mul1.wat` file:

<!-- $MDX file=mul1.wat -->
```wat
(module

  (func (export "unused1") (param $x i32)
    local.get $x
    drop
  )

  (func (export "mul") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.mul
  )
)
```

And the following `mul2.wat` file:


<!-- $MDX file=mul2.wat -->
```wat
(module

  (func (export "unused2") (param $x i32) (param $y i64) (result i64)
    local.get $x
    (if (then (unreachable)))
    local.get $y
  )

  (func (export "mul") (param $x i32) (param $y i32) (result i32)
    local.get $y
    local.get $x
    i32.mul
    i32.const 1
    i32.add
  )
)
```

Owi can find an input for which the `mul` function of these two modules is not equivalent:

```sh
$ owi iso ./mul1.wat ./mul2.wat
owi: [ERROR] Assert failure: (bool.eq (i32.mul symbol_0 symbol_1)
                              (i32.add (i32.mul symbol_1 symbol_0) 1))
model {
  symbol symbol_0 i32 0
  symbol symbol_1 i32 0
}
owi: [ERROR] Reached problem!
[13]
```

## Man page

```sh
$ owi iso --help=plain
NAME
       owi-iso - Check the iso-functionnality of two Wasm modules by
       comparing the output when calling their exports.

SYNOPSIS
       owi iso [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of auto, always or never.

       --deterministic-result-order
           Guarantee a fixed deterministic order of found failures. This
           implies --no-stop-at-failure.

       --fail-on-assertion-only
           ignore traps and only report assertion violations

       --fail-on-trap-only
           ignore assertion violations and only report traps

       --model-format=VAL (absent=scfg)
            The format of the model ("json" or "scfg")

       --model-out-file=FILE
           Output the generated model to FILE. if --no-stop-at-failure is
           given this is used as a prefix and the ouputed files would have
           PREFIX_%d.

       --no-assert-failure-expression-printing
           do not display the expression in the assert failure

       --no-stop-at-failure
           do not stop when a program failure is encountered

       --no-value
           do not display a value for each symbol

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -s VAL, --solver=VAL (absent=Z3)
           SMT solver to use

       -u, --unsafe
           skip typechecking pass

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning or OWI_VERBOSITY env)
           Be more or less verbose. LEVEL must be one of quiet, error,
           warning, info or debug. Takes over -v.

       -w VAL, --workers=VAL (absent=n)
           number of workers for symbolic execution. Defaults to the number
           of physical cores.

       --with-breadcrumbs
           add breadcrumbs to the generated model

       --workspace=DIR
           write results and intermediate compilation artifacts to dir

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi iso exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

ENVIRONMENT
       These environment variables affect the execution of owi iso:

       OWI_VERBOSITY
           See option --verbosity.

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
