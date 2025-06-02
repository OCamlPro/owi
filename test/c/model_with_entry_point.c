#include <owi.h>

int fun() {
  int i = owi_i32();
  owi_assume(i < 2);
  owi_assume(i > 0);
  owi_assert(i == 2);
  return i;
}

int main() {
  int i = owi_i32();
  owi_assume(i < 3);
  owi_assume(i > 1);
  owi_assert(i == 3);
  return i;
}
