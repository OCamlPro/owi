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
  deque_add_last(deque, &c);
  deque_add_last(deque, &d);
  deque_add_last(deque, &e);
  deque_add_last(deque, &f);

  deque_add_at(deque, &g, 4);

  const void *const *buff = deque_get_buffer(deque);

  owi_assert(buff[4] == &g);

  owi_assert(buff[5] == &e);

  const void *elem = buff[6];
  owi_assert(elem == &f);

  teardown_tests();
  return 0;
}
