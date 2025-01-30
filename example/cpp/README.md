# Performing symbolic execution on C++ programs

## Solving polynomials

Given the following `poly.cpp` file:

<!-- $MDX file=poly.cpp -->
```cpp
#include <owi.h>

/*
extern "C" {
  void _start() {
    owi_abort();
  }
}
*/


int main() {
  int x = owi_i32();
  int x2 = x * x;
  int x3 = x * x * x;

  int a = 1;
  int b = -7;
  int c = 14;
  int d = -8;

  int poly = a * x3 + b * x2 + c * x + d;

  owi_assert(poly != 0);

  return 0;
}
```

We are defining one symbolic variable `x` using the function `owi_i32(void)`. Then we build a polynomial `poly` equal to $x^3 - 7x^2 + 14x - 8$.

Then we use `owi_assert(poly != 0)`. Which should fail as this polynomial has multiple roots. Let's see what owi says about it:

```sh
$ owi c++ ./poly.cpp -w1 --no-assert-failure-expression-printing
wasm-ld-19: error: entry symbol not defined (pass --no-entry to suppress): main
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
clang++ failed: run with --debug to get the full error message
[26]
```

Indeed, `4` is a root of the polynomial and thus it is expected to be equal to `0` in this case. We know the three roots are `1`, `2` and `4`, so let's inform owi that we are not interested in this cases.

We can do so by assuming that `x` is not equal to any of these with the function `owi_assume(bool)`:

<!-- $MDX file=poly2.cpp -->
```cpp
#include <owi.h>

int main() {
  int x = owi_i32();
  int x2 = x * x;
  int x3 = x * x * x;

  int a = 1;
  int b = -7;
  int c = 14;
  int d = -8;

  int poly = a * x3 + b * x2 + c * x + d;

  owi_assume(x != 1);
  owi_assume(x != 2);
  owi_assume(x != 4);

  // Make model output deterministic
  owi_assume(x > -2147483646);

  owi_assert(poly != 0);

  return 0;
}
```

Let's run owi on this new input:


```sh
$ owi c++ ./poly2.cpp --no-assert-failure-expression-printing
wasm-ld-19: error: entry symbol not defined (pass --no-entry to suppress): main
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
clang++ failed: run with --debug to get the full error message
[26]
```

And indeed, `-2147483644` is a root of the polynomial! Well, not quite…

Remember that we are working on 32 bits integers here. Thus *overflows* are a thing we have to think about. And indeed when `x` is equal to `-2147483644`, because of overflows, the polynomial will be equal to zero.

Exercise: can you find another "root" of the polynomial ? :-)

## Man page

```sh
$ owi c++ --help=plain
NAME
       owi-c++ - Compile a C++ file to Wasm and run the symbolic interpreter
       on it

SYNOPSIS
       owi c++ [OPTION]… [ARG]…

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
       owi c++ exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
