#include <owi.h>

int main(int argc, char **argv) {
  static char array[5];
  char c = owi_char();
  for (int i = 0; i < sizeof(array); i++) {
    if (c == owi_char()) {
      return i;
    }
  }

  owi_assert(0);

  return 0;
}
