#include "queue.h"
#include "owi.h"

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

  owi_assert(2 == queue_size(q));

  void *p;
  queue_peek(q, &p);
  owi_assert(&a == p);

  queue_enqueue(q, &c);

  queue_peek(q, &p);
  owi_assert(&a == p);

  teardown_test();
  return 0;
}
