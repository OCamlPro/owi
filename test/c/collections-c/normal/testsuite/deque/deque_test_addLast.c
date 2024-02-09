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

  owi_assert(3 == deque_size(deque));

  const void *const *u = deque_get_buffer(deque);
  const void *e = u[0];

  owi_assert(e == &a);

  e = u[1];
  owi_assert(e == &b);

  e = u[2];
  owi_assert(e == &c);

  teardown_tests();
  return 0;
}
