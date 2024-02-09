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

  array_remove_at(v1, 2, NULL);

  owi_assert(3 == array_size(v1));

  int *r;
  array_get_at(v1, 2, (void *)&r);

  owi_assert(r == &e);

  array_destroy(v1);

  return 0;
}
