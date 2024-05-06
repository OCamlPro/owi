# Concolic interpreter

## Basic example

The symbolic interpreter allows to define *symbolic variables* whose values is considered to be any possible value. Then, we collect informations during the execution and when an error is reached, we try to see if there's a possible value for all the symbolic variables by taking all our informations into account.

In the following file, we define `x` as a symbolic variable. Then if `5 < x`, we fail.

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

Let's see if owi is able to find a value for `x` that lead to an error:

```sh
$ owi conc ./mini.wat
Trap: unreachable
Model:
  (model
    (symbol_1 (i32 6)))
Reached problem!
[13]
```

Indeed, if `x` is equal to `6` then, the `unreachable` instruction will be reached.

## Man page

```sh
$ owi conc --help=plain
NAME
       owi-conc - Run the concolic interpreter

SYNOPSIS
       owi conc [OPTION]… [ARG]…

OPTIONS
       -d, --debug
           debug mode

       --deterministic-result-order
           Guarantee a fixed deterministic order of found failures. This
           implies --no-stop-at-failure.

       --no-stop-at-failure
           do not stop when a program failure is encountered

       --no-value
           do not display a value for each symbol

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

       -s VAL, --solver=VAL (absent=Z3)
           SMT solver to use

       -u, --unsafe
           skip typechecking pass

       -w VAL, --workers=VAL (absent=n)
           number of workers for symbolic execution. Defaults to the number
           of physical cores.

       --workspace=VAL (absent=owi-out)
           path to the workspace directory

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi conc exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
