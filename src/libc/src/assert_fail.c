#include <owi.h>

void __assert_fail(const char *assertion, const char *file, unsigned int line,
                   const char *function) {
  owi_assert(0);
}
