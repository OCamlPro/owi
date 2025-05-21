#include <owi.h>

int main() {
  int x = owi_int();
  int x2 = x * x;

  owi_assert(x2 != 0);

  return 0;
}
