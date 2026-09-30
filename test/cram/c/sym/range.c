#include <owi.h>

int main() {
  int x = owi_range(10, 20);
  owi_assert(x >= 10);
  owi_assert(x < 20);

  int y = owi_range(100, 200);
  owi_assert(y >= 101);
  owi_assert(y < 200);
}