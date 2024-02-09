#include "array.h"
#include "owi.h"

static Array *v1;
static Array *v2;
static ArrayConf vc;
static int stat;

int main() {
  stat = array_new(&v1);
  int n = owi_i32();
  owi_assume(n > 2);
  owi_assume(n < 16);

  int *last;
  int *next_to_last;

  for (int i = 0; i < n; i++) {
    int *a = malloc(sizeof(int));
    array_add(v1, a);
    next_to_last = last;
    last = a;
  }

  array_remove_all(v1);

  owi_assert(array_size(v1) == 0);

  array_destroy(v1);

  return 0;
}
