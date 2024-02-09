#include "owi.h"
#include "treeset.h"
#include "utils.h"

static TreeSet *set;

int main() {
  treeset_new(cmp, &set);

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();
  owi_assume(a != b && a != c && b != c);

  treeset_add(set, &a);
  treeset_add(set, &b);
  treeset_add(set, &c);

  owi_assert(3 == treeset_size(set));
}
