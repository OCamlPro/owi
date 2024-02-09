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
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();

  slist_add(list, &a);
  slist_add(list, &b);
  slist_add(list, &c);
  slist_add(list, &d);

  size_t idx;
  slist_index_of(list, &a, &idx);
  owi_assert(0 == idx);

  slist_index_of(list, &c, &idx);
  owi_assert(2 == idx);
  ;

  teardown_test();
  return 0;
}
