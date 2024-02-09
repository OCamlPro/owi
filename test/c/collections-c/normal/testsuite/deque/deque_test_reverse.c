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

  deque_add(deque, &a);
  deque_add(deque, &b);
  deque_add(deque, &c);

  deque_reverse(deque);

  int *ra;
  deque_get_at(deque, 0, (void *)&ra);
  int *rb;
  deque_get_at(deque, 1, (void *)&rb);
  int *rc;
  deque_get_at(deque, 2, (void *)&rc);

  owi_assert(c == *ra);
  owi_assert(b == *rb);
  owi_assert(a == *rc);

  teardown_tests();
  return 0;
}
