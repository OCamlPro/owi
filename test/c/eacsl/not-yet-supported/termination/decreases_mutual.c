#include <owi.h>

int odd(int);
int even(int);

/*@ requires  n >= 0;
    decreases n;
*/
int even(int n) {
    if (n == 0) return 1;
    return odd(n - 1);
}

/*@ requires  n >= 0;
    decreases n;
*/
int odd(int n) {
    if (n == 0) return 0;
    return even(n - 1);
}

int main(void) {
    int n = owi_i32();
    owi_assume(n >= 0);
    owi_assume(n <= 10);
    even(n);
    return 0;
}
