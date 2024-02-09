#include "array.h"
#include "owi.h"

static Array *v1;
static Array *v2;
static ArrayConf vc;
static int stat;

int main() {
  stat = array_new(&v1);

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();

  array_add(v1, &a);
  array_add(v1, &b);
  array_add(v1, &c);
  array_add(v1, &d);
  owi_assume(c != a && c != b && c != d);

  ArrayIter iter;
  array_iter_init(&iter, v1);

  int *e;
  while (array_iter_next(&iter, (void *)&e) != CC_ITER_END) {
    if (*e == c)
      array_iter_remove(&iter, NULL);
  }

  owi_assert(0 == array_contains(v1, &c));

  array_destroy(v1);

  return 0;
}
