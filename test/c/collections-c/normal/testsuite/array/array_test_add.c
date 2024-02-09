#include "array.h"
#include "owi.h"

static Array *v1;
static Array *v2;
static ArrayConf vc;
static int stat;

int main() {
  stat = array_new(&v1);
  owi_assert(stat == CC_OK);

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();

  array_add(v1, &a);
  array_add(v1, &b);
  array_add(v1, &c);

  int *ar;
  int *br;
  int *cr;

  array_get_at(v1, 0, (void *)&ar);
  array_get_at(v1, 1, (void *)&br);
  array_get_at(v1, 2, (void *)&cr);

  owi_assert(a == *ar);
  owi_assert(b == *br);
  owi_assert(c == *cr);

  array_destroy(v1);

  return 0;
}
