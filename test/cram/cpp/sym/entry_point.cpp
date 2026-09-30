#include <owi.h>

extern "C" int fun() {
  int x = owi_range(10, 20);
  owi_assert(x >= 10);
  owi_assert(x < 19);
  return x;
}
