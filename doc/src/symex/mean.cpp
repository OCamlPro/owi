#include <owi.h>

int mean1(int x, int y) {
  return (x & y) + ((x ^ y) >> 1);
}

int mean2(int x, int y) {
  return (x + y) / 2;
}

extern "C" void check(int x, int y) {
  owi_assert(mean1(x, y) == mean2(x, y));
}
