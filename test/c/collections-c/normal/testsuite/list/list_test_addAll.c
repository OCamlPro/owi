#include "list.h"
#include "owi.h"

static List *list1;
static List *list2;

int a, b, c, d, e, f, g, h;

void setup_tests() {
  list_new(&list1), list_new(&list2);

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

  list_add(list1, va);
  list_add(list1, vb);
  list_add(list1, vc);
  list_add(list1, vd);

  va = (int *)malloc(sizeof(int));
  vb = (int *)malloc(sizeof(int));
  vc = (int *)malloc(sizeof(int));
  vd = (int *)malloc(sizeof(int));

  *va = e;
  *vb = f;
  *vc = g;
  *vd = h;

  list_add(list2, va);
  list_add(list2, vb);
  list_add(list2, vc);
  list_add(list2, vd);
}

void teardown_test() {
  list_destroy_cb(list1, free);
  list_destroy(list2);
}

int main() {
  setup_tests();

  list_add_all(list2, list1);

  owi_assert(8 == list_size(list2));

  int *l1last;
  list_get_last(list1, (void *)&l1last);

  int *l2last;
  list_get_last(list2, (void *)&l2last);
  owi_assert(*l1last == *l2last);

  teardown_test();
}
