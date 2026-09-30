#include <owi.h>

int main() {
    _Bool b1 = owi_bool(), b2 = owi_bool();
    owi_assume(b1 || b2);
    owi_assume(b1 || !b2);
    owi_assert(b1);
    return 0;
}
