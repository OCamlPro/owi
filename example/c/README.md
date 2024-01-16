# Performing symbolic execution on C programs

## Solving polynomials example

Given the following `poly.c` file:

<!-- $MDX file=poly.c -->
```wast
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

  owi_assert(poly != 0);

  return 0;
}
```

We are defining one symbolic variable `x` using the function `owi_i32(void)`. Then we build a polynomial `poly` equal to $x^3 - 7x^2 + 14x - 8$.

Then we use `owi_assert(poly != 0)`. Which should fail as this polynomial has multiple roots. Let's see what owi says about it:

```sh
$ dune exec owi -- c ./poly.c
...
Model:
  (model
    (symbol_0 i32 (i32 1)))
Reached problem!
```

Indeed, `1` is a root of the polynomial and thus it is expected to be equal to `0` in this case. We know the three roots are `1`, `2` and `4`, so let's inform owi that we are not interested in this cases.

We can do so by assuming that `x` is not equal to any of these with the function `owi_assume(bool)`:

<!-- $MDX file=poly2.c -->
```wast
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

  owi_assert(poly != 0);

  return 0;
}
```

Let's run owi on this new input:


```sh
$ dune exec owi -- c ./poly2.c
...
Model:
  (model
    (symbol_0 i32 (i32 -2147483644)))
Reached problem!
```

And indeed, `-2147483644` is a root of the polynomial! Well, not quite…

Remember that we are working on 32 bits integers here. Thus *overflows* are a thing we have to think about. And indeed when `x` is equal to `-2147483644`, because of overflows, the polynomial will be equal to zero.

Exercise: can you find another "root" of the polynomial ? :-)

## Man page

```sh
$ dune exec owi -- c --help
OWI-C(1)                          Owi Manual                          OWI-C(1)

NAME
       owi-c - Compile a C file to Wasm and run the symbolic interpreter on it

SYNOPSIS
       owi c [OPTION]… [ARG]…

OPTIONS
       -d, --debug
           debug mode

       -I VAL
           headers path

       -m VAL, --arch=VAL (absent=32)
           data model

       -o VAL, --output=VAL (absent=owi-out)
           write results to dir

       -O VAL (absent=0)
           specify which optimization level to use

       -p VAL, --property=VAL
           property file

       --testcomp
           test-comp mode

       -w VAL, --workers=VAL (absent=4)
           number of workers for symbolic execution

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show  this  help  in format FMT. The value FMT must be one of auto,
           pager, groff or plain. With auto, the  format  is  pager  or  plain
           whenever the TERM env var is dumb or undefined.

       --version
           Show version information.

EXIT STATUS
       owi c exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

Owi 11VERSION11                                                       OWI-C(1)
```
