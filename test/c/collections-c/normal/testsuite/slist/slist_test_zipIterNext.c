#include "owi.h"
#include "slist.h"
#include "utils.h"

static SList *list;
static SList *list2;
static int stat;

void setup_test() {
  stat = slist_new(&list);
  slist_new(&list2);
};

void teardown_test() {
  slist_destroy(list);
  slist_destroy(list2);
};

int main() {
  setup_test();

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

  slist_add(list, str_a);
  slist_add(list, str_b);
  slist_add(list, str_c);
  slist_add(list, str_d);

  slist_add(list2, str_e);
  slist_add(list2, str_f);
  slist_add(list2, str_g);

  SListZipIter zip;
  slist_zip_iter_init(&zip, list, list2);

  size_t i = 0;

  void *e1, *e2;
  while (slist_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (i == 0) {
      CHECK_EQUAL_C_STRING(str_a, (char *)e1);
      CHECK_EQUAL_C_STRING(str_e, (char *)e2);
    }
    if (i == 2) {
      CHECK_EQUAL_C_STRING(str_c, (char *)e1);
      CHECK_EQUAL_C_STRING(str_g, (char *)e2);
    }
    i++;
  }
  owi_assert(3 == i);

  teardown_test();
  return 0;
}
