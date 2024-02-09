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

  int *last;
  deque_get_last(deque, (void *)&last);

  owi_assert(b == *last);

  teardown_tests();
  return 0;
}
