#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

int main() {
    owi_open_scope("scope 1");
    int sym = owi_i32();
    owi_end_scope();
    owi_assert(0);
    return 0;
}
