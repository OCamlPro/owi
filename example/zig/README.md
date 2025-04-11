# Performing symbolic execution on Zig programs

## Fibo example

Given the following `fib.zig` file:

<!-- $MDX file=fib.zig -->
```zig
// TODO: replace this by a proper include of the owi header?
extern "symbolic" fn i32_symbol() i32;
extern "symbolic" fn assume(bool) void;
extern "symbolic" fn assert(bool) void;

fn fibonacci(n: i32) i32 {
    if (n < 0) {
        @panic("expected a positive number");
    }
    if (n <= 2) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

pub fn main() void {
    const n: i32 = i32_symbol();
    assume(n > 0);
    assume(n < 10);
    const result = fibonacci(n);
    assert(result != 21);
}
```

Owi can find a crash with:

```sh
$ owi zig ./fib.zig -w1 --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32 7
}
Reached problem!
[13]
```

## Man page

```sh
$ owi zig --help=plain
NAME
       owi-zig - Compile a Zig file to Wasm and run the symbolic interpreter
       on it

SYNOPSIS
       owi zig [OPTION]… FILE…

ARGUMENTS
       FILE (required)
           source files

OPTIONS
       --concolic
           concolic mode

       -d, --debug
           debug mode

       --deterministic-result-order
           Guarantee a fixed deterministic order of found failures. This
           implies --no-stop-at-failure.

       --entry-point=FUNCTION
           entry point of the executable

       --fail-on-assertion-only
           ignore traps and only report assertion violations

       --fail-on-trap-only
           ignore assertion violations and only report traps

       -I VAL
           headers path

       --invoke-with-symbols
           Invoke the entry point of the program with symbolic values instead
           of dummy constants.

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

       -o FILE, --output=FILE
           Output the generated .wasm or .wat to FILE.

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
       owi zig exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
