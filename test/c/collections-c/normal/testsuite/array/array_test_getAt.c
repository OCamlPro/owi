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
  int e = owi_i32();

  array_add(v1, &a);
  array_add(v1, &b);
  array_add(v1, &c);
  array_add(v1, &e);

  int *ar;
  int *cr;
  array_get_at(v1, 0, (void *)&ar);
  array_get_at(v1, 2, (void *)&cr);

  owi_assert(a == *ar);
  owi_assert(c == *cr);

  array_destroy(v1);

  return 0;
}
