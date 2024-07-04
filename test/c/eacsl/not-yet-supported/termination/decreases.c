#include <owi.h>

/*@ requires  n >= 0;
    decreases n;
*/
int fact(int n) {
    if (n == 0) return 1;
    return n * fact(n - 1);
}

int main(void) {
    int n = owi_i32();
    owi_assume(n >= 0);
    owi_assume(n <= 10);
    fact(n);
    return 0;
}
