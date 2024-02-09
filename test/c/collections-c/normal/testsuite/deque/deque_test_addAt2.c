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
  int d = owi_i32();
  int e = owi_i32();
  int f = owi_i32();
  int g = owi_i32();

  deque_add_last(deque, &a);
  deque_add_last(deque, &b);
  deque_add_first(deque, &c);
  deque_add_first(deque, &d);
  deque_add_first(deque, &e);
  deque_add_first(deque, &f);

  deque_add_at(deque, &g, 1);

  const void *const *buff = deque_get_buffer(deque);
  const void *elem = buff[5];

  owi_assert(elem == &g);

  const void *elem1 = buff[0];
  owi_assert(elem1 == &a);

  const void *elem2 = buff[7];
  owi_assert(elem2 == &c);

  const void *elem3 = buff[3];
  owi_assert(elem3 == &f);

  teardown_tests();
  return 0;
}
