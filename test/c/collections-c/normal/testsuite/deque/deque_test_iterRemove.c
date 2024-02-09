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

  deque_add(deque, &a);
  deque_add(deque, &b);
  deque_add(deque, &c);
  deque_add(deque, &d);
  deque_add(deque, &e);
  deque_add(deque, &f);

  owi_assume(d != a && d != b && d != c && d != e && d != f);

  DequeIter iter;
  deque_iter_init(&iter, deque);

  size_t i = 0;
  void *el;
  while (deque_iter_next(&iter, &el) != CC_ITER_END) {
    if (i == 3)
      deque_iter_remove(&iter, NULL);

    if (i > 2) {
      owi_assert(5 == deque_size(deque));
    } else {
      owi_assert(6 == deque_size(deque));
    }
    if (i >= 3) {
      owi_assert(i - 1 == deque_iter_index(&iter));
    }
    i++;
  }

  teardown_tests();
  return 0;
}
