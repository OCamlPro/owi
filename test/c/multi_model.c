#include <owi.h>

int main() {
    _Bool nondet = owi_bool();
    if (nondet) {
        owi_assert(0);
    } else {
        owi_assert(0);
    }
    return 0;
}
