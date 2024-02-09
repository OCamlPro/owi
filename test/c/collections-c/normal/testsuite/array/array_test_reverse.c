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

  array_add(v1, &a);
  array_add(v1, &b);
  array_add(v1, &c);

  array_reverse(v1);

  int *i0;
  int *i1;
  int *i2;
  array_get_at(v1, 0, (void *)&i0);
  array_get_at(v1, 1, (void *)&i1);
  array_get_at(v1, 2, (void *)&i2);

  owi_assert(c == *i0);
  owi_assert(b == *i1);
  owi_assert(a == *i2);

  array_destroy(v1);

  return 0;
}
