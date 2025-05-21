#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

int main() {
    int sym = owi_int();

    owi_open_scope("scope 1");
    int sym1 = owi_int();
    owi_close_scope();

    owi_open_scope("scope 2");
    int sym2 = owi_int();
    int sym3 = owi_int();
    int sym4 = owi_int();
    owi_close_scope();

    owi_open_scope("scope 3");
    owi_open_scope("scope 3.1");
    int sym5 = owi_int();
    owi_close_scope();
    owi_open_scope("scope 3.2");
    int sym6 = owi_int();
    int sym7 = owi_int();
    owi_close_scope();
    owi_close_scope();

    owi_open_scope("aaa");
    int x = owi_int();
    owi_close_scope();

    owi_open_scope("aaa");
    int y = owi_int();
    owi_close_scope();
    if (x == 42 && y != 42) {
        owi_assert(0);
    }

    return 0;
}
