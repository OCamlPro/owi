#include <owi.h>

/*@ requires y != 0;
    behavior yes:
      assumes x % y == 0;
      ensures \result == 1;
    behavior no:
      assumes x % y != 0;
      ensures \result == 1; */
int is_dividable(int x, int y) {
  return x % y == 0;
}

int main(void) {
  int numerator = owi_i32();
  int denominator = owi_i32();

  owi_assume(denominator != 0);

  is_dividable(numerator, denominator);
  return 0;
}
