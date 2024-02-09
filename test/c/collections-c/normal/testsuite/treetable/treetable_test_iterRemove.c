#include "owi.h"
#include "treetable.h"
#include "utils.h"

static TreeTable *table;

int main() {
  treetable_new(cmp, &table);

  int x = owi_i32();
  int y = owi_i32();
  int z = owi_i32();

  owi_assume(x < y && y < z);

  int a = owi_i32();

  char str_a[] = {a, '\0'};

  int b = owi_i32();

  char str_b[] = {b, '\0'};

  int c = owi_i32();

  char str_c[] = {c, '\0'};

  treetable_add(table, &x, str_a);
  treetable_add(table, &y, str_b);
  treetable_add(table, &z, str_c);

  TreeTableIter iter;
  treetable_iter_init(&iter, table);

  TreeTableEntry entry;
  while (treetable_iter_next(&iter, &entry) != CC_ITER_END) {
    int const *key = entry.key;

    if (*key == y) {
      owi_assert(CC_OK == treetable_iter_remove(&iter, NULL));

      owi_assert(CC_ERR_KEY_NOT_FOUND == treetable_iter_remove(&iter, NULL));
    }
  }

  owi_assert(2 == treetable_size(table));
  owi_assert(0 == treetable_contains_key(table, &y));

  treetable_destroy(table);
}
