#include "array.h"
#include "owi.h"
#include "utils.h"

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

  array_copy_deep(v1, copy, &v2);

  owi_assert(array_size(v2) == array_size(v1));

  int *ca;
  array_get_at(v2, 0, (void *)&ca);

  owi_assert(a == *ca);
  array_destroy_cb(v2, free);

  array_destroy(v1);

  return 0;
}
