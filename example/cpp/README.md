# Performing symbolic execution on C++ programs

## Solving polynomials

Given the following `poly.cpp` file:

<!-- $MDX file=poly.cpp -->
```cpp
#include <owi.h>

class Poly {
private:
  int poly;
public:
  Poly(int a, int b, int c, int d) {
    int x = owi_i32();
    int x2 = x * x;
    int x3 = x2 * x;
    poly = a*x3 + b*x2 + c*x + d;
  }

  int hasRoot() const { return poly == 0; }
};

int main() {
  Poly p(1, -7, 14, -8);
  owi_assert(not(p.hasRoot()));
}
```

We are defining one symbolic variable `x` using the function `owi_i32(void)`. Then we build a polynomial `poly` equal to $x^3 - 7x^2 + 14x - 8$.

Then we use `owi_assert(p.getPoly() != 0)`. Which should fail as this polynomial has multiple roots. Let's see what owi says about it:

```sh
$ owi c++ ./poly.cpp -w1 --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32 4
}
Reached problem!
[13]
```

Indeed, `4` is a root of the polynomial and thus it is expected to be equal to `0` in this case. We know the three roots are `1`, `2` and `4`, so let's inform owi that we are not interested in this cases.

We can do so by assuming that `x` is not equal to any of these with the function `owi_assume(bool)`:

<!-- $MDX file=poly2.cpp -->
```cpp
#include <owi.h>

class Poly {
private:
  int poly;
public:
  Poly(int a, int b, int c, int d) {
    int x = owi_i32();
    int x2 = x * x;
    int x3 = x2 * x;
    owi_assume(x != 1);
    owi_assume(x != 2);
    // make model output deterministic
    owi_assume(x > -2147483646);
    owi_assume(x != 4);
    poly = a*x3 + b*x2 + c*x + d;
  }

  int hasRoot() const { return poly == 0; }
};

int main() {
  Poly p(1, -7, 14, -8);
  owi_assert(not(p.hasRoot()));
}
```

Let's run owi on this new input:


```sh
$ owi c++ ./poly2.cpp --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32 -2147483644
}
Reached problem!
[13]
```

And indeed, `-2147483644` is a root of the polynomial! Well, not quite…

Remember that we are working on 32 bits integers here. Thus *overflows* are a thing we have to think about. And indeed when `x` is equal to `-2147483644`, because of overflows, the polynomial will be equal to zero.

Exercise: can you find another "root" of the polynomial ? :-)

## Checking the equivalence of two functions

<!-- $MDX file=mean.cpp -->
```cpp
#include <owi.h>

struct IntPair {
  int x, y;
  int mean1() const { return (x & y) + ((x ^ y) >> 1); }
  int mean2() const { return (x + y) / 2; }
};

int main() {
  IntPair p{owi_i32(), owi_i32()};
  owi_assert(p.mean1() == p.mean2());
}
```

```sh
$ owi c++ ./mean.cpp --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32 -2147483648
  symbol symbol_1 i32 -2147483646
}
Reached problem!
[13]
```

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
