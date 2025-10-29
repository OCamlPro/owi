#include <owi.h>

int main() {
  int x = owi_int();
  int x2 = x * x;
  int x3 = x * x * x;

  int a = 1;
  int b = -7;
  int c = 14;
  int d = -8;

  int poly = a * x3 + b * x2 + c * x + d;

  owi_assume(x != 1);
  owi_assume(x != 2);
  owi_assume(x != 4);

  // Make model output deterministic
  owi_assume(x > -2147483646);

  owi_assert(poly != 0);

  return 0;
}
