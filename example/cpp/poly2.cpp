#include <owi.h>

class Poly {
  public:
    int poly;
    Poly(int a, int b, int c, int d) {
      int x = owi_i32();
      int x2 = x * x;
      int x3 = x * x * x;
      poly = a * x3 + b * x2 + c * x + d;
      owi_assume(x != 1);
      owi_assume(x != 2);
      // Make model output deterministic
      owi_assume(x > -2147483646);
      owi_assume(x != 4);
    }
    int getPoly() {
      return this->poly;
    }
};

int main() {
  Poly p(1, -7, 14, -8);

  owi_assert(p.getPoly() != 0);

  return 0;
}
