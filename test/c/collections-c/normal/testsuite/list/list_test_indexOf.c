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
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();

  owi_assume(a != b && a != c && a != d && b != c && b != d && c != d);

  list_add(list1, &a);
  list_add(list1, &b);
  list_add(list1, &c);
  list_add(list1, &d);

  size_t idx;
  list_index_of(list1, &a, zero_if_ptr_eq, &idx);
  owi_assert(0 == idx);

  list_index_of(list1, &c, zero_if_ptr_eq, &idx);
  owi_assert(2 == idx);

  teardown_test();
}
