# Performing symbolic execution on Rust programs

## Small example

Given the following `main.rs` file:

<!-- $MDX file=main.rs -->
```rs
use owi_sym::Symbolic;

fn mean_wrong(x: i32, y: i32) -> i32 {
    (x + y) / 2
}

fn mean_correct(x: i32, y: i32) -> i32 {
    (x & y) + ((x ^ y) >> 1)
}

fn main() {
    let x = i32::symbol();
    let y = i32::symbol();
    owi_sym::assert(mean_wrong(x, y) == mean_correct(x, y))
}
```

Let's check if the two functions are the same for any input:

```sh
$ owi rust ./main.rs -w1 --fail-on-assertion-only --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32 1073741835
  symbol symbol_1 i32 -2147483642
}
Reached problem!
[13]
```

Indeed, in this case, there will be an integer overflow in one of the two functions and thus they won't give the same result.

## Man page

```sh
$ owi rust --help=plain
NAME
       owi-rust - Compile a Rust file to Wasm and run the symbolic
       interpreter on it

SYNOPSIS
       owi rust [OPTION]… [ARG]…

OPTIONS
       --concolic
           concolic mode

       -d, --debug
           debug mode

       --deterministic-result-order
           Guarantee a fixed deterministic order of found failures. This
           implies --no-stop-at-failure.

       --fail-on-assertion-only
           ignore traps and only report assertion violations

       --fail-on-trap-only
           ignore assertion violations and only report traps

       -I VAL
           headers path

       -m VAL, --arch=VAL (absent=32)
           data model

       --model-output-format=VAL (absent=scfg)
           The format of the output model

       --no-assert-failure-expression-printing
           do not display the expression in the assert failure

       --no-stop-at-failure
           do not stop when a program failure is encountered

       --no-value
           do not display a value for each symbol

       -O VAL (absent=3)
           specify which optimization level to use

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

       --profile=FILE
           Profile file.

       -s VAL, --solver=VAL (absent=Z3)
           SMT solver to use

       -u, --unsafe
           skip typechecking pass

       -w VAL, --workers=VAL (absent=n)
           number of workers for symbolic execution. Defaults to the number
           of physical cores.

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the format is pager or plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi rust exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
