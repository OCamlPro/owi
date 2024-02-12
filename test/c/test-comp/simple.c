#include <owi.h>

int main() {
  int x = owi_i32();
  int x2 = x * x;

  owi_assert(x2 != 0);

  return 0;
}
