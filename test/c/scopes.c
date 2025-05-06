#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

int main() {
    int sym = owi_i32();

    owi_open_scope("scope 1");
    int sym1 = owi_i32();
    owi_close_scope();

    owi_open_scope("scope 2");
    int sym2 = owi_i32();
    int sym3 = owi_i32();
    int sym4 = owi_i32();
    owi_close_scope();

    owi_assert(0);

    return 0;
}
