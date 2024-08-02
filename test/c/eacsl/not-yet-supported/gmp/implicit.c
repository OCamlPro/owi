#include <limits.h>
#include <stdlib.h>
#include <owi.h>

/*@ requires n >= 0;
    requires \valid(p + (0 .. n - 1));
    assigns  \nothing;
    ensures  \result == \sum(0, n - 1, \lambda integer i; p[i] * p[i]);
*/
int sqsum(int* p, int n) {
    int S = 0, tmp;
    /*@ loop invariant 0 <= i <= n;
        loop invariant S == \sum(0, i - 1, \lambda integer j; p[j] * p[j]);
        loop assigns S, tmp;
        loop variant n - i;
    */
    for (int i = 0; i < n; ++i) {
        //@ assert p[i] * p[i] <= INT_MAX;
        tmp = p[i] * p[i];
        //@ assert S + tmp <= INT_MAX;
        S += tmp;
    }
    return S;
}

int main(void) {
    int size = 10;
    int* p = malloc(size * sizeof(int));
    sqsum(p, size);
    return 0;
}
