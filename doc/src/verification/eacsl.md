# E-ACSL

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

    int n = owi_int();
    owi_assume(n >= 2);
    owi_assume(n <= MAX_SIZE);

    primes(is_prime, n);
    free(is_prime);
    return 0;
}
```

```sh
$ owi c --e-acsl primes.c -w1
owi: [ERROR] Assert failure: false
model {
  symbol symbol_0 i32 2
}
owi: [ERROR] Reached problem!
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

    int n = owi_int();
    owi_assume(n >= 2);
    owi_assume(n <= MAX_SIZE);

    primes(is_prime, n);
    free(is_prime);
    return 0;
}
```

```sh
$ # owi c --e-acsl primes2.c -w1
```

All the specified properties have been satisfied during the execution.


