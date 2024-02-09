#include "deque.h"
#include "owi.h"
#include "utils.h"

static Deque *deque;
static DequeConf conf;
int stat;

void setup_tests() { stat = deque_new(&deque); }

void teardown_tests() { deque_destroy(deque); }

int main() {
  setup_tests();

  int *a = malloc(sizeof(int));
  int *b = malloc(sizeof(int));
  int *c = malloc(sizeof(int));

  int x = owi_i32();
  int y = owi_i32();
  int z = owi_i32();

  *a = x;
  *b = y;
  *c = z;

  deque_add_last(deque, a);
  deque_add_last(deque, b);
  deque_add_last(deque, c);

  Deque *cpy;
  deque_copy_deep(deque, copy, &cpy);

  int size = deque_size(cpy);
  owi_assert(3 == size);

  int *ca;
  deque_get_at(cpy, 0, (void *)&ca);
  int *cb;
  deque_get_at(cpy, 1, (void *)&cb);
  int *cc;
  deque_get_at(cpy, 2, (void *)&cc);

  owi_assert(x == *ca);
  owi_assert(y == *cb);
  owi_assert(z == *cc);
  deque_destroy_cb(cpy, free);
  free(a);
  free(b);
  free(c);

  teardown_tests();
  return 0;
}
