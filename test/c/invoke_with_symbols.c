#include <owi.h>

void f(int n, long long int m, float x, double y) {
    owi_assume(n == 1);
    owi_assume(y == -1234.);
    owi_assume(n > m);
    owi_assume(x > y);
    owi_assert(x != n);
}
