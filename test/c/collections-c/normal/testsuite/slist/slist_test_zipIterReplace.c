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

  int h = owi_i32();
  owi_assume(h > 0);
  owi_assume(h < 127);
  char str_h[] = {h, '\0'};

  int i = owi_i32();
  owi_assume(i > 0);
  owi_assume(i < 127);
  char str_i[] = {i, '\0'};

  int x = owi_i32();
  owi_assume(x > 0);
  owi_assume(x < 127);
  char str_x[] = {x, '\0'};

  int y = owi_i32();
  owi_assume(y > 0);
  owi_assume(y < 127);
  char str_y[] = {y, '\0'};

  owi_assume(b != a && b != c && b != d);
  owi_assume(h != a && h != b && h != c && h != d);
  owi_assume(i != e && i != f && i != g);

  slist_add(list, str_a);
  slist_add(list, str_b);
  slist_add(list, str_c);
  slist_add(list, str_d);

  slist_add(list2, str_e);
  slist_add(list2, str_f);
  slist_add(list2, str_g);

  SListZipIter zip;
  slist_zip_iter_init(&zip, list, list2);

  void *e1, *e2;
  void *r1, *r2;
  while (slist_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (strcmp((char *)e1, str_b) == 0)
      slist_zip_iter_replace(&zip, str_h, str_i, &r1, &r2);
  }

  size_t index;
  slist_index_of(list, str_h, &index);
  owi_assert(1 == index);

  slist_index_of(list, str_i, &index);
  owi_assert(1 == index);
  owi_assert(1 == slist_contains(list, str_h));
  owi_assert(1 == slist_contains(list2, str_i));

  teardown_test();
  return 0;
}
