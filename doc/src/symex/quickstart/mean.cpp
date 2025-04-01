#include <owi.h>

struct IntPair {
  int x, y;
  int mean1() const { return (x & y) + ((x ^ y) >> 1); }
  int mean2() const { return (x + y) / 2; }
};

int main() {
  IntPair p{owi_i32(), owi_i32()};
  owi_assert(p.mean1() == p.mean2());
}
