#include <owi.h>

int main(int argc, char **argv) {
  static char array[5];
  char c = owi_i8();
  for (int i = 0; i < sizeof(array); i++) {
    if (c == owi_i8()) {
      return i;
    }
  }

  owi_assert(0);

  return 0;
}
