#include <owi.h>
#include <stdlib.h>

int main() {
  int size = owi_i32();
  owi_assume(size < 10);
  owi_assume(size > 0);
  int* ptr = (int*) malloc(size);
  free(ptr);
  owi_assert(0);
  return ptr;
}
