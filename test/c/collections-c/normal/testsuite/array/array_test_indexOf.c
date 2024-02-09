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

  size_t ai;
  array_index_of(v1, &a, &ai);

  size_t ci;
  array_index_of(v1, &c, &ci);

  owi_assert(0 == ai);
  owi_assert(2 == ci);

  array_destroy(v1);

  return 0;
}
