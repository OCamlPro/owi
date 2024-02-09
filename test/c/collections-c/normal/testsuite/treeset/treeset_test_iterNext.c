#include "owi.h"
#include "treeset.h"
#include "utils.h"

static TreeSet *set;

int main() {
  treeset_new(cmp, &set);

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();
  owi_assume(a < b && b < c && c < d);

  treeset_add(set, &a);
  treeset_add(set, &b);
  treeset_add(set, &c);
  treeset_add(set, &d);

  int one = 0;
  int two = 0;
  int three = 0;
  int four = 0;

  TreeSetIter iter;
  treeset_iter_init(&iter, set);

  void *e;
  while (treeset_iter_next(&iter, &e) != CC_ITER_END) {
    if (*((int *)e) == a)
      one++;

    if (*((int *)e) == b)
      two++;

    if (*((int *)e) == c)
      three++;

    if (*((int *)e) == d)
      four++;
  }

  owi_assert(1 == one);
  owi_assert(1 == two);
  owi_assert(1 == three);
  owi_assert(1 == four);
}
