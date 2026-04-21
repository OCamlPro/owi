#include <stdlib.h>

void f(void) {
  void *a = malloc(10);
  free(a);
  free(a);
}
