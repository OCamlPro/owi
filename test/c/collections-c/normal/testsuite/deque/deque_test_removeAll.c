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

  deque_remove_all(deque);

  void *first;
  int stat1 = deque_get_first(deque, &first);
  void *last;
  int stat2 = deque_get_last(deque, &last);

  owi_assert(CC_ERR_OUT_OF_RANGE == stat1);
  owi_assert(CC_ERR_OUT_OF_RANGE == stat2);

  owi_assert(0 == deque_size(deque));

  teardown_tests();
  return 0;
}
