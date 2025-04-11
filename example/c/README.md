# Performing symbolic execution on C programs

## Solving polynomials

Given the following `poly.c` file:

<!-- $MDX file=poly.c -->
```c
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
$ owi c ./poly.c -w1 --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32 4
}
Reached problem!
[13]
```

Indeed, `4` is a root of the polynomial and thus it is expected to be equal to `0` in this case. We know the three roots are `1`, `2` and `4`, so let's inform owi that we are not interested in this cases.

We can do so by assuming that `x` is not equal to any of these with the function `owi_assume(bool)`:

<!-- $MDX file=poly2.c -->
```c
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
$ owi c ./poly2.c --no-assert-failure-expression-printing
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

## Solving a maze

<!-- $MDX file=maze.c -->
```c
#include <owi.h>

// example from https://feliam.wordpress.com/2010/10/07/the-symbolic-maze/

#define H 7
#define W 11
#define ITERS 28

char maze[H][W] = {
  "+-+---+---+",
  "| |     |#|",
  "| | --+ | |",
  "| |   | | |",
  "| +-- | | |",
  "|     |   |",
  "+-----+---+"
};

int main (void) {

  int x = 1;
  int y = 1;
  maze[y][x]='X';

  char program[ITERS];

  for (int i = 0; i < ITERS; i++) {
    program[i] = owi_i32();
  }

  int old_x = x;
  int old_y = y;

  for (int i = 0; i < ITERS; i++) {

    old_x = x;
    old_y = y;

    switch (program[i]) {
      case 'w':
        y--;
        break;
      case 's':
        y++;
        break;
      case 'a':
        x--;
        break;
      case 'd':
        x++;
        break;
      default:
        return 1;
    }

    if (maze[y][x] == '#') {
      // TODO: print the result
      owi_assert(0);
      return 0;
    }

    if (maze[y][x] != ' ' && !((y == 2 && maze[y][x] == '|' && x > 0 && x < W))) {
      return 1;
    }

    if (old_x == x && old_y == y) {
      return 1;
    }

    maze[y][x] = 'X';
  }
  return 1;
}
```

```sh
$ owi c ./maze.c --no-value --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32
  symbol symbol_1 i32
  symbol symbol_10 i32
  symbol symbol_11 i32
  symbol symbol_12 i32
  symbol symbol_13 i32
  symbol symbol_14 i32
  symbol symbol_15 i32
  symbol symbol_16 i32
  symbol symbol_17 i32
  symbol symbol_18 i32
  symbol symbol_19 i32
  symbol symbol_2 i32
  symbol symbol_20 i32
  symbol symbol_21 i32
  symbol symbol_22 i32
  symbol symbol_23 i32
  symbol symbol_24 i32
  symbol symbol_25 i32
  symbol symbol_26 i32
  symbol symbol_27 i32
  symbol symbol_3 i32
  symbol symbol_4 i32
  symbol symbol_5 i32
  symbol symbol_6 i32
  symbol symbol_7 i32
  symbol symbol_8 i32
  symbol symbol_9 i32
}
Reached problem!
[13]
```

## Dobble example

<!-- $MDX file=dobble.c -->
```c
// An encoding representing the problem of finding a suitable
// set of cards for https://en.wikipedia.org/wiki/Dobble.
// Cards are encoded on integers, with each position
// representing one of N_CARDS possible symbols.
#include <owi.h>
#include <stdlib.h>

// Number of symbols per card
#define CARD_SIZE 3

#define N_CARDS ((CARD_SIZE*CARD_SIZE) - CARD_SIZE + 1)

int popcount(unsigned int x) {
    int count = 0;
    for (int i = 0; i < N_CARDS; i++) {
        count += x & 1;
        x >>= 1;
    }
    return count;
}

int main() {
    unsigned int cards[N_CARDS];
    for (int i=0;i < N_CARDS; i++) {
        unsigned int x = owi_i32();
        owi_assume((x >> N_CARDS) == 0);
        owi_assume(popcount(x) == CARD_SIZE);
        cards[i] = x;
        if (i > 0) {
            owi_assume(cards[i] > cards[i-1]);
        }
    }
    unsigned int acc = 1;
    for (int i=0;i < N_CARDS; i++) {
        for(int j=i+1; j < N_CARDS;j++) {
            owi_assume(cards[i] != cards[j]);
            unsigned int z = cards[i] & cards[j];
            acc = acc & (z != 0);
            acc = acc & ((z & (z-1)) == 0);
        }
    }
    owi_assert(!acc);
}
```

<!-- TODO: remove `-O1` once symbolic popcnt is implemented -->
```sh
$ owi c -O1 ./dobble.c -w1 --no-value --no-assert-failure-expression-printing
Assert failure
model {
  symbol symbol_0 i32
  symbol symbol_1 i32
  symbol symbol_2 i32
  symbol symbol_3 i32
  symbol symbol_4 i32
  symbol symbol_5 i32
  symbol symbol_6 i32
}
Reached problem!
[13]
```

# Combining symbolic execution with runtime assertion checking (RAC)

E-ACSL is a specification language of C codes, as well as a runtime assertion checking tool within Frama-C. It works by consuming a C program annotated with E-ACSL specifications, it generates a monitored C program which aborts its execution when the specified properties are violated at runtime.

Generally, such a C program runs on concrete values. Yet we can combine symbolic execution with runtime assertion checking, in order to check the properties using symbolic values. This will lead to better coverage of potential execution paths and scenarios.

## Finding primes

Consider the following (faulty) function `primes`, it implements the algorithm of the **Sieve of Eratosthenes** to find all the prime numbers smaller than `n`:

```c
void primes(int *is_prime, int n) {
    for (int i = 1; i < n; ++i) is_prime[i] = 1;
    for (int i = 2; i * i < n; ++i) {
        if (!is_prime[i]) continue;
        for (int j = i; i * j < n; ++j) {
            is_prime[i * j] = 0;
        }
    }
}
```

Initially, it marks each number as prime. It then marks as composite the multiples of each prime, iterating in an ascending order. If a number is still marked as prime at the point of iteration, then it does not admit a nontrivial factor and should be a prime.

In order to verify the implementation, we annotate the function `primes` using the E-ACSL specification language. The annotations should be written immediately above the function and surrounded by `/*@ ... */`.

```c
#define MAX_SIZE 100

/*@ requires 2 <= n <= MAX_SIZE;
    requires \valid(is_prime + (0 .. (n - 1)));
    ensures  \forall integer i; 0 <= i < n ==>
        (is_prime[i] <==>
            (i >= 2 && \forall integer j; 2 <= j < i ==> i % j != 0));
*/
void primes(int *is_prime, int n) {
    for (int i = 0; i < n; ++i) is_prime[i] = 1;
    for (int i = 2; i * i < n; ++i) {
        if (!is_prime[i]) continue;
        for (int j = i; i * j < n; ++j) {
            is_prime[i * j] = 0;
        }
    }
}
```

Here, `requires` and `ensures` specify the pre-condition and post-condition of the function. The annotation means:
- When the function is called,
  + the argument `n` should be between `2` and `MAX_SIZE`
  + for all `i` between `0` and `n - 1`, `is_prime + i` should be memory locations safe to read and write
- When the function returns,
  + for all `i` between `0` and `n - 1`, `is_prime[i]` evaluates to `true` if and only if `i` is larger than `2` and does not have a factor between `2` and `i - 1` (which indicates the primality of `i`)

We can then call the function with symbolic values and see what happens. We should pass the option `--e-acsl` to let owi invoke the E-ACSL plugin.

<!-- $MDX file=primes.c -->
```c
#define MAX_SIZE 100

#include <owi.h>
#include <stdlib.h>

/*@ requires 2 <= n <= MAX_SIZE;
    requires \valid(is_prime + (0 .. (n - 1)));
    ensures  \forall integer i; 0 <= i < n ==>
        (is_prime[i] <==>
            (i >= 2 && \forall integer j; 2 <= j < i ==> i % j != 0));
*/
void primes(int *is_prime, int n) {
    for (int i = 0; i < n; ++i) is_prime[i] = 1;
    for (int i = 2; i * i < n; ++i) {
        if (!is_prime[i]) continue;
        for (int j = i; i * j < n; ++j) {
            is_prime[i * j] = 0;
        }
    }
}

int main(void) {
    int *is_prime;
    is_prime = malloc(MAX_SIZE * sizeof(int));

    int n = owi_i32();
    owi_assume(n >= 2);
    owi_assume(n <= MAX_SIZE);

    primes(is_prime, n);
    free(is_prime);
    return 0;
}
```

```sh
$ owi c --e-acsl primes.c -w1
Assert failure: false
model {
  symbol symbol_0 i32 2
}
Reached problem!
[13]
```

The execution got aborted because one of the specifications has been violated with `n = 2`. (The error message is not so informative for the time being, extra information aiding the diagnostic of errors may be added in the future.)

The problem is that we should mark `0` and `1` as non-prime during the initialization. Let's fix it and rerun the program.

<!-- $MDX file=primes2.c -->
```c
#define MAX_SIZE 100

#include <owi.h>
#include <stdlib.h>

/*@ requires 2 <= n <= MAX_SIZE;
    requires \valid(is_prime + (0 .. (n - 1)));
    ensures  \forall integer i; 0 <= i < n ==>
        (is_prime[i] <==>
            (i >= 2 && \forall integer j; 2 <= j < i ==> i % j != 0));
*/
void primes(int *is_prime, int n) {
    for (int i = 0; i < n; ++i) is_prime[i] = 1;
    is_prime[0] = is_prime[1] = 0;
    for (int i = 2; i * i < n; ++i) {
        if (!is_prime[i]) continue;
        for (int j = i; i * j < n; ++j) {
            is_prime[i * j] = 0;
        }
    }
}

int main(void) {
    int *is_prime;
    is_prime = malloc(MAX_SIZE * sizeof(int));

    int n = owi_i32();
    owi_assume(n >= 2);
    owi_assume(n <= MAX_SIZE);

    primes(is_prime, n);
    free(is_prime);
    return 0;
}
```

```sh
$ owi c --e-acsl primes2.c
All OK
```

All the specified properties have been satisfied during the execution.

## Man page

```sh
$ owi c --help=plain
NAME
       owi-c - Compile a C file to Wasm and run the symbolic interpreter on
       it

SYNOPSIS
       owi c [OPTION]… FILE…

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

       --e-acsl
           e-acsl mode, refer to
           https://frama-c.com/download/e-acsl/e-acsl-implementation.pdf for
           Frama-C's current language feature implementations

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

       -m VAL, --arch=VAL (absent=32)
           data model

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

       -O VAL (absent=3)
           specify which optimization level to use

       --optimize
           optimize mode

       -p, --profiling
           profiling mode

       --profile=FILE
           Profile file.

       --property=FILE
           property file

       -s VAL, --solver=VAL (absent=Z3)
           SMT solver to use

       --testcomp
           test-comp mode

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
       owi c exits with:

       0   on success.

       123 on indiscriminate errors reported on standard error.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

BUGS
       Email them to <contact@ndrs.fr>.

SEE ALSO
       owi(1)

```
