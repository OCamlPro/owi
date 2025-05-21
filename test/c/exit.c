#include <stdlib.h>
#include <owi.h>

int main(void) {
    int s = owi_int();
    if (s) {
      exit(0);
    }
    owi_assert(s);
}
