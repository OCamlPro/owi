#include "deque.h"
#include "owi.h"

static Deque *deque;
static DequeConf conf;
int stat;

void setup_tests() { stat = deque_new(&deque); }

void teardown_tests() { deque_destroy(deque); }

bool pred1(const void *e) { return *(int *)e <= 3; }

bool pred2(const void *e) { return *(int *)e > 3; }

bool pred3(const void *e) { return *(int *)e > 5; }

int main() {
  setup_tests();

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();
  int e = owi_i32();
  int f = owi_i32();

  owi_assume(pred2(&d) && pred2(&e) && pred2(&f) && !pred2(&a) && !pred2(&b) &&
             !pred2(&c));

  deque_add_last(deque, &a);
  deque_add_last(deque, &b);
  deque_add_last(deque, &c);
  deque_add_last(deque, &d);
  deque_add_last(deque, &e);
  deque_add_last(deque, &f);
  owi_assert(6 == deque_size(deque));

  Deque *filter = NULL;
  deque_filter(deque, pred2, &filter);
  const void *const *buff = deque_get_buffer(filter);

  owi_assert(3 == deque_size(filter));
  owi_assert(buff[0] == &d);
  owi_assert(buff[1] == &e);
  owi_assert(buff[2] == &f);

  free(filter);

  teardown_tests();
  return 0;
}
