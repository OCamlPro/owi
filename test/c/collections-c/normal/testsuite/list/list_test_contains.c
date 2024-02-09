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
  int e = owi_i32();

  owi_assume(a != b && a != c && a != d && a != e && b != c && b != d &&
             b != e && c != d && c != e && d != e);

  list_add(list1, &a);
  list_add(list1, &b);
  list_add(list1, &b);
  list_add(list1, &c);
  list_add(list1, &d);

  owi_assert(2 == list_contains(list1, &b));
  owi_assert(1 == list_contains(list1, &d));
  owi_assert(0 == list_contains(list1, &e));

  teardown_test();
}
