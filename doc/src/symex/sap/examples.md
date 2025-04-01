# Examples of Problem Solving

## Solving polynomials

<details>
  <summary>C</summary>

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
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 4
}
owi: [ERROR] Reached problem!
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
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -2147483644
}
owi: [ERROR] Reached problem!
[13]
```

And indeed, `-2147483644` is a root of the polynomial! Well, not quite…

Remember that we are working on 32 bits integers here. Thus *overflows* are a thing we have to think about. And indeed when `x` is equal to `-2147483644`, because of overflows, the polynomial will be equal to zero.

Exercise: can you find another "root" of the polynomial ? :-)

</details>

<details>
  <summary>C++</summary>

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
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 4
}
owi: [ERROR] Reached problem!
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
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32 -2147483644
}
owi: [ERROR] Reached problem!
[13]
```

And indeed, `-2147483644` is a root of the polynomial! Well, not quite…

Remember that we are working on 32 bits integers here. Thus *overflows* are a thing we have to think about. And indeed when `x` is equal to `-2147483644`, because of overflows, the polynomial will be equal to zero.

Exercise: can you find another "root" of the polynomial ? :-)

</details>

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
owi: [ERROR] Assert failure
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
owi: [ERROR] Reached problem!
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
owi: [ERROR] Assert failure
model {
  symbol symbol_0 i32
  symbol symbol_1 i32
  symbol symbol_2 i32
  symbol symbol_3 i32
  symbol symbol_4 i32
  symbol symbol_5 i32
  symbol symbol_6 i32
}
owi: [ERROR] Reached problem!
[13]
```

