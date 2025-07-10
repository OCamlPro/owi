#include <owi.h>

void f(int n, long int m, float x, double y) {
    owi_assume(n > m);
    owi_assume(m > x);
    owi_assume(x > y);
    owi_assume(y > n);
    // this should never be reached because the products of previous assumes is unsat
    owi_assert(0);
}
