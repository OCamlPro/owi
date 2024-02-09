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

  char str_a[] = {a, '\0'};

  int b = owi_i32();

  char str_b[] = {b, '\0'};

  int c = owi_i32();

  char str_c[] = {c, '\0'};

  int d = owi_i32();

  char str_d[] = {d, '\0'};

  int e = owi_i32();

  char str_e[] = {e, '\0'};

  int f = owi_i32();

  char str_f[] = {f, '\0'};

  int g = owi_i32();

  char str_g[] = {g, '\0'};

  owi_assume((!(strcmp(str_a, str_b) == 0)) && (!(strcmp(str_c, str_b) == 0)) &&
             (!(strcmp(str_d, str_b) == 0)));

  array_add(v1, str_a);
  array_add(v1, str_b);
  array_add(v1, str_c);
  array_add(v1, str_d);

  array_new(&v2);

  array_add(v2, str_e);
  array_add(v2, str_f);
  array_add(v2, str_g);

  ArrayZipIter zip;
  array_zip_iter_init(&zip, v1, v2);

  void *e1, *e2;
  void *r1, *r2;
  while (array_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (strcmp((char *)e1, str_b) == 0)
      array_zip_iter_remove(&zip, &r1, &r2);
  }
  CHECK_EQUAL_C_STRING(str_b, (char *)r1);
  owi_assert(0 == array_contains(v1, str_b));
  owi_assert(0 == array_contains(v2, str_f));
  owi_assert(3 == array_size(v1));
  owi_assert(2 == array_size(v2));

  array_destroy(v2);

  array_destroy(v1);

  return 0;
}
