#include "owi.h"
#include "slist.h"

static SList *list;
static SList *list2;
static int stat;

int a, b, c, d, e, f, g, h;

void setup_test() {
  slist_new(&list), slist_new(&list2);

  a = owi_i32();
  b = owi_i32();
  c = owi_i32();
  d = owi_i32();
  e = owi_i32();
  f = owi_i32();
  g = owi_i32();
  h = owi_i32();

  int *va = (int *)malloc(sizeof(int));
  int *vb = (int *)malloc(sizeof(int));
  int *vc = (int *)malloc(sizeof(int));
  int *vd = (int *)malloc(sizeof(int));

  *va = a;
  *vb = b;
  *vc = c;
  *vd = d;

  slist_add(list, va);
  slist_add(list, vb);
  slist_add(list, vc);
  slist_add(list, vd);

  va = (int *)malloc(sizeof(int));
  vb = (int *)malloc(sizeof(int));
  vc = (int *)malloc(sizeof(int));
  vd = (int *)malloc(sizeof(int));

  *va = e;
  *vb = f;
  *vc = g;
  *vd = h;

  slist_add(list2, va);
  slist_add(list2, vb);
  slist_add(list2, vc);
  slist_add(list2, vd);
};

void teardown_test() {
  slist_destroy(list);
  slist_destroy(list2);
};

int main() {
  setup_test();

  SList *cp;
  slist_copy_shallow(list, &cp);
  owi_assert(4 == slist_size(cp));

  void *e1;
  void *e2;

  slist_get_first(cp, &e1);
  slist_get_first(list, &e2);
  owi_assert(e1 == e2);

  slist_get_last(cp, &e1);
  slist_get_last(list, &e2);
  owi_assert(e1 == e2);

  slist_get_at(cp, 3, &e1);
  slist_get_at(list, 3, &e2);
  owi_assert(e1 == e2);

  slist_destroy(cp);

  teardown_test();
  return 0;
}
