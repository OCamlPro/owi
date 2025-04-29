#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

int main() {
    int normal_bool = owi_bool();
    int invisible_bool = owi_invisible_bool();
    owi_assert(0);
}
