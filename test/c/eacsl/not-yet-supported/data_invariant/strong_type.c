#include <owi.h>

typedef int temperature;
//@ strong type invariant temperature_positive(temperature t) = t > 0;

void not_decrement(temperature *t, int n) {
    *t -= n;
    *t += n;
}

int main(void) {
    temperature t = 20;
    int n = owi_int();
    not_decrement(&t, n);
    return 0;
}
