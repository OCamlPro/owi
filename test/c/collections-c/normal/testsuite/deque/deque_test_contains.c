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

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();
  int e = owi_i32();
  int f = owi_i32();
  int g = owi_i32();

  deque_add(deque, &a);
  deque_add(deque, &b);
  deque_add(deque, &c);
  deque_add(deque, &d);
  deque_add(deque, &e);
  deque_add(deque, &f);
  deque_add(deque, &a);

  owi_assert(2 == deque_contains(deque, &a));
  owi_assert(0 == deque_contains(deque, &g));
  owi_assert(1 == deque_contains(deque, &e));

  teardown_tests();
  return 0;
}
