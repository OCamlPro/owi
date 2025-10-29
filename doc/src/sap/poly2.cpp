#include <owi.h>

class Poly {
private:
  int poly;
public:
  Poly(int a, int b, int c, int d) {
    int x = owi_int();
    int x2 = x * x;
    int x3 = x2 * x;
    owi_assume(x != 1);
    owi_assume(x != 2);
    // make model output deterministic
    owi_assume(x > -2147483646);
    owi_assume(x != 4);
    poly = a*x3 + b*x2 + c*x + d;
  }

  int hasRoot() const { return poly == 0; }
};

int main() {
  Poly p(1, -7, 14, -8);
  owi_assert(not(p.hasRoot()));
}
