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
