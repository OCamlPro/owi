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
  char str_a[] = {a, '\0'};

  int b = owi_i32();
  char str_b[] = {b, '\0'};

  int c = owi_i32();
  char str_c[] = {c, '\0'};

  int d = owi_i32();
  char str_d[] = {d, '\0'};

  owi_assert(CC_OK == list_add(list1, str_a));
  owi_assert(CC_OK == list_add(list1, str_b));
  owi_assert(CC_OK == list_add(list1, str_c));
  owi_assert(CC_OK == list_add(list1, str_d));

  void *e;
  list_get_first(list1, &e);
  owi_assert(e != NULL);

  list_get_last(list1, &e);
  owi_assert(e != NULL);

  teardown_test();
}
