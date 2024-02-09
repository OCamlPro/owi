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

  void *e;
  deque_get_at(deque, 1, &e);
  void *n;
  int status = deque_get_at(deque, 42, &n);

  owi_assert(b == *(int *)e);
  owi_assert(CC_ERR_OUT_OF_RANGE == status);

  teardown_tests();
  return 0;
}
