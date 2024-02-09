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

  owi_assume(!pred3(&d) && !pred3(&e) && pred3(&f) && !pred3(&a) &&
             !pred3(&b) && !pred3(&c));

  deque_add_last(deque, &a);
  deque_add_last(deque, &b);
  deque_add_last(deque, &c);
  deque_add_last(deque, &d);
  deque_add_last(deque, &e);
  deque_add_last(deque, &f);
  owi_assert(6 == deque_size(deque));

  Deque *filter = NULL;
  deque_filter(deque, pred3, &filter);
  const void *const *buff = deque_get_buffer(filter);

  owi_assert(1 == deque_size(filter));
  owi_assert(buff[0] == &f);

  free(filter);

  teardown_tests();
  return 0;
}
