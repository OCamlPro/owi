#include "owi.h"
#include "treetable.h"
#include "utils.h"

static TreeTable *table;

int main() {
  treetable_new(cmp, &table);

  int x = owi_i32();
  int y = owi_i32();
  int z = owi_i32();
  int w = owi_i32();

  int a = owi_i32();

  char str_a[] = {a, '\0'};

  int b = owi_i32();

  char str_b[] = {b, '\0'};

  int c = owi_i32();

  char str_c[] = {c, '\0'};

  int d = owi_i32();

  char str_d[] = {d, '\0'};

  owi_assume(x < y && y < z && z < w);

  treetable_add(table, &z, str_a);
  treetable_add(table, &w, str_b);
  treetable_add(table, &y, str_c);
  treetable_add(table, &x, str_d);

  int *last;
  treetable_get_last_key(table, (void *)&last);

  owi_assert(w == *last);

  treetable_destroy(table);
}
