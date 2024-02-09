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
  int e = owi_i32();
  int f = owi_i32();
  slist_add(list, &a);
  slist_add(list, &b);
  slist_add(list, &c);
  slist_add(list, &d);
  slist_add(list, &e);
  slist_add(list, &f);

  slist_reverse(list);

  int reverse_ar[] = {f, e, d, c, b, a};

  SListIter i;
  slist_iter_init(&i, list);

  void *el;
  int ind = 0;
  while (slist_iter_next(&i, &el) != CC_ITER_END) {
    owi_assert(reverse_ar[ind] == *(int *)el);
    ind++;
  }

  teardown_test();
  return 0;
}
