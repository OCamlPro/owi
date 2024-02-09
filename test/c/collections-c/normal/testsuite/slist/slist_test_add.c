#include "owi.h"
#include "slist.h"

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

  char str_a[] = {a, '\0'};

  int b = owi_i32();

  char str_b[] = {b, '\0'};

  int c = owi_i32();

  char str_c[] = {c, '\0'};

  int d = owi_i32();

  char str_d[] = {d, '\0'};

  owi_assert(CC_OK == slist_add(list, str_a));
  owi_assert(CC_OK == slist_add(list, str_b));
  owi_assert(CC_OK == slist_add(list, str_c));
  owi_assert(CC_OK == slist_add(list, str_d));

  void *e;
  slist_get_first(list, &e);
  owi_assert(e != NULL);

  slist_get_last(list, &e);
  owi_assert(e != NULL);

  teardown_test();
  return 0;
}
