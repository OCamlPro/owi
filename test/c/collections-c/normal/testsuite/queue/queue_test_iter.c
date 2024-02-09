#include "owi.h"
#include "queue.h"

static Queue *q;
static Queue *q2;
static int stat;

void setup_test() {
  stat = queue_new(&q);
  queue_new(&q2);
}

void teardown_test() {
  queue_destroy(q);
  queue_destroy(q2);
}

int main() {
  setup_test();

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();

  queue_enqueue(q, &a);
  queue_enqueue(q, &b);
  queue_enqueue(q, &c);

  size_t x = 0;
  size_t y = 0;
  size_t z = 0;

  QueueIter iter;
  queue_iter_init(&iter, q);

  int *e;
  while (queue_iter_next(&iter, (void *)&e) != CC_ITER_END) {
    if (e == &a)
      x++;

    if (e == &b)
      y++;

    if (e == &c)
      z++;
  }

  owi_assert(1 == x);
  owi_assert(1 == y);
  owi_assert(1 == z);

  teardown_test();
  return 0;
}
