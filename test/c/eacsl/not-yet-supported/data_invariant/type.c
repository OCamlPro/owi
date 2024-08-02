#include <owi.h>

typedef int temperature;
//@ type invariant temperature_positive(temperature t) = t > 0;

void decrement(temperature *t, int n) {
    *t -= n;
}

int main(void) {
    temperature t = 20;
    int n = owi_i32();
    decrement(&t, n);
    return 0;
}
