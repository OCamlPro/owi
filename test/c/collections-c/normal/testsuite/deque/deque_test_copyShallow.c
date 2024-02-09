#include "deque.h"
#include "owi.h"

static Deque *deque;
static DequeConf conf;
int stat;

void setup_tests() { stat = deque_new(&deque); }

void teardown_tests() { deque_destroy(deque); }

int main() {
  setup_tests();

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();

  deque_add_last(deque, &a);
  deque_add_last(deque, &b);
  deque_add_last(deque, &c);

  Deque *copy;
  deque_copy_shallow(deque, &copy);

  int size = deque_size(copy);
  owi_assert(3 == size);

  int *ca;
  deque_get_at(copy, 0, (void *)&ca);

  int *cb;
  deque_get_at(copy, 1, (void *)&cb);

  int *cc;
  deque_get_at(copy, 2, (void *)&cc);

  owi_assert(a == *ca);
  owi_assert(b == *cb);
  owi_assert(c == *cc);
  deque_destroy(copy);

  teardown_tests();
  return 0;
}
