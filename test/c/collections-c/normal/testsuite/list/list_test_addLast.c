#include "list.h"
#include "owi.h"

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
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();
  int p = owi_i32();

  list_add(list1, &a);
  list_add(list1, &b);
  list_add(list1, &c);
  list_add(list1, &d);

  owi_assert(4 == list_size(list1));

  int *last;
  list_get_last(list1, (void *)&last);
  owi_assert(d == *last);

  list_add_last(list1, &p);
  owi_assert(5 == list_size(list1));

  list_get_last(list1, (void *)&last);
  owi_assert(p == *last);

  teardown_test();
}
