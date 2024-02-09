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

  deque_add_first(deque, &a);
  deque_add_last(deque, &b);
  deque_add_first(deque, &c);

  int *first;
  deque_get_first(deque, (void *)&first);

  owi_assert(c == *first);

  teardown_tests();
  return 0;
}
