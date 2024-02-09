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

  owi_assume(a != b && a != c && a != d && a != h && a != x);
  owi_assume(b != c && b != d && b != h && b != x);
  owi_assume(c != d && c != h && c != x);
  owi_assume(d != h && d != x && h != x);

  owi_assume(e != f && e != g && e != i && e != y && f != g && f != i &&
             f != y && g != i && g != y && i != y);

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
  while (list_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (strcmp((char *)e1, str_b) == 0)
      list_zip_iter_add(&zip, str_h, str_i);
  }

  size_t index;
  owi_assert(CC_OK == list_index_of(list1, str_h, zero_if_ptr_eq, &index));
  owi_assert(2 == index);

  owi_assert(CC_OK == list_index_of(list2, str_i, zero_if_ptr_eq, &index));
  owi_assert(2 == index);

  owi_assert(CC_OK == list_index_of(list1, str_c, zero_if_ptr_eq, &index));
  owi_assert(3 == index);

  owi_assert(1 == list_contains(list1, str_h));
  owi_assert(1 == list_contains(list2, str_i));
  owi_assert(5 == list_size(list1));
  owi_assert(4 == list_size(list2));

  list_zip_iter_init(&zip, list1, list2);
  while (list_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (strcmp((char *)e2, str_g) == 0)
      list_zip_iter_add(&zip, str_x, str_y);
  }

  char *last;
  list_get_last(list1, (void *)&last);
  CHECK_EQUAL_C_STRING(str_d, last);

  list_get_last(list2, (void *)&last);
  CHECK_EQUAL_C_STRING(str_y, last);

  teardown_test();
}
