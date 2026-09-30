#include <stdlib.h>
#include <owi.h>

int main(void) {
    int s = owi_int();
    if (s) {
      abort();
    }
    owi_assert(s);
}
