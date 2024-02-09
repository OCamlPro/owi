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

  owi_assume(!pred1(&d) && !pred1(&e) && !pred1(&f) && pred1(&a) && pred1(&b) &&
             pred1(&c));

  deque_add_last(deque, (void *)&a);
  deque_add_last(deque, (void *)&b);
  deque_add_last(deque, (void *)&c);
  deque_add_last(deque, (void *)&d);
  deque_add_last(deque, (void *)&e);
  deque_add_last(deque, (void *)&f);
  owi_assert(6 == deque_size(deque));

  Deque *filter = NULL;
  deque_filter(deque, pred1, &filter);
  owi_assert(3 == deque_size(filter));
  const void *const *buff = deque_get_buffer(filter);

  owi_assert(buff[0] == &a);
  owi_assert(buff[1] == &b);

  const void *elem = buff[2];
  owi_assert(elem == &c);
  free(filter);

  teardown_tests();
  return 0;
}
