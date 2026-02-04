#include <owi.h>

int f(int x) {

  int arr[4] = {1, 2, 0, 4};

  if (x >= 0 && x < 4) {
    return 10 / arr[x];
  }

  return -1;
}

extern "C" int check(void) {
  int x = owi_int();
  return f(x);
}
