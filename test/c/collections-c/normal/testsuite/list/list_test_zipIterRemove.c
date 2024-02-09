#include "list.h"
#include "owi.h"
#include "utils.h"

static List *list1;
static List *list2;

void setup_tests() { list_new(&list1), list_new(&list2); }

void teardown_test() {
  list_destroy(list1);
  list_destroy(list2);
}

int main() {
  setup_tests();

  int a = owi_i32();
  owi_assume(a > 0);
  owi_assume(a < 127);
  char str_a[] = {a, '\0'};

  int b = owi_i32();
  owi_assume(b > 0);
  owi_assume(b < 127);
  char str_b[] = {b, '\0'};

  int c = owi_i32();
  owi_assume(c > 0);
  owi_assume(c < 127);
  char str_c[] = {c, '\0'};

  int d = owi_i32();
  owi_assume(d > 0);
  owi_assume(d < 127);
  char str_d[] = {d, '\0'};

  int e = owi_i32();
  owi_assume(e > 0);
  owi_assume(e < 127);
  char str_e[] = {e, '\0'};

  int f = owi_i32();
  owi_assume(f > 0);
  owi_assume(f < 127);
  char str_f[] = {f, '\0'};

  int g = owi_i32();
  owi_assume(g > 0);
  owi_assume(g < 127);
  char str_g[] = {g, '\0'};

  owi_assume(a != b && a != c && a != d && a != e && a != f && a != g &&
             b != c && b != d && b != e && b != f && b != g && c != d &&
             c != e && c != f && c != g && d != e && d != f && d != g &&
             e != f && e != g && f != g);

  list_add(list1, str_a);
  list_add(list1, str_b);
  list_add(list1, str_c);
  list_add(list1, str_d);

  list_add(list2, str_e);
  list_add(list2, str_f);
  list_add(list2, str_g);

  ListZipIter zip;
  list_zip_iter_init(&zip, list1, list2);

  void *e1, *e2;
  void *r1, *r2;
  while (list_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (strcmp((char *)e1, str_b) == 0)
      list_zip_iter_remove(&zip, &r1, &r2);
  }
  CHECK_EQUAL_C_STRING(str_b, (char *)r1);
  CHECK_EQUAL_C_STRING(str_f, (char *)r2);
  owi_assert(0 == list_contains(list1, str_b));
  owi_assert(0 == list_contains(list2, str_f));
  owi_assert(3 == list_size(list1));
  owi_assert(2 == list_size(list2));

  teardown_test();
}
