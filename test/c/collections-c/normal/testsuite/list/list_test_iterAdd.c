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

  int i = owi_i32();
  int *ins = (int *)malloc(sizeof(int));
  *ins = i;

  owi_assume(i != d);

  ListIter iter;
  list_iter_init(&iter, list1);

  owi_assume(c != a && c != b && c != d && d != a && d != b);

  int *el;
  while (list_iter_next(&iter, (void *)&el) != CC_ITER_END) {
    if (*el == c)
      list_iter_add(&iter, ins);
  }

  owi_assert(5 == list_size(list1));

  int *li3;
  list_get_at(list1, 3, (void *)&li3);

  owi_assert(*li3 == *ins);

  int *li4;
  list_get_at(list1, 4, (void *)&li4);
  owi_assert(d == *li4);

  list_iter_init(&iter, list1);

  int x = owi_i32();
  ins = (int *)malloc(sizeof(int));
  *ins = x;

  while (list_iter_next(&iter, (void *)&el) != CC_ITER_END) {
    if (*el == d) {
      list_iter_add(&iter, ins);
    }
  }

  void *e;
  list_get_last(list1, &e);
  owi_assert(*ins == *((int *)e));

  teardown_test();
}
