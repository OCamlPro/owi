#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

int main() {
    _Bool normal_bool = owi_bool();
    _Bool invisible_bool = owi_invisible_bool();
    owi_assert(0);
}
