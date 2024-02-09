#include "owi.h"
#include "pqueue.h"

static struct Pair {
  int a, b;
} A, B, C;

static int comp(const void *a, const void *b) {
  int alpha1 = ((struct Pair *)a)->a, beta1 = ((struct Pair *)a)->b;
  int alpha2 = ((struct Pair *)b)->a, beta2 = ((struct Pair *)b)->b;
  if (alpha1 != alpha2)
    return alpha1 - alpha2;
  else
    return beta1 - beta2;
}

static int comp2(const void *a, const void *b) {
  return *((int *)a) - *((int *)b);
}

static PQueue *p1, *p2;

void setup_tests() {
  pqueue_new(&p1, comp2);
  PQueueConf cfg;
  pqueue_conf_init(&cfg, comp);
  pqueue_new_conf(&cfg, &p2);
}

void teardown_tests() {
  pqueue_destroy(p1);
  pqueue_destroy(p2);
}

int main() {
  setup_tests();

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();
  int d = owi_i32();

  int e = owi_i32();
  int f = owi_i32();
  int *ptr;

  owi_assume(e < 8388608 && e > -8388608);
  owi_assume(f < 8388608 && f > -8388608);

  pqueue_push(p1, (void *)&f);
  pqueue_top(p1, (void *)&ptr);
  owi_assert(&f == ptr);

  pqueue_push(p1, (void *)&e);
  pqueue_top(p1, (void *)&ptr);
  owi_assert(((e > f) && (e == *ptr)) || ((e <= f) && (f == *ptr)));

  owi_assume(a < 8388608 && a > -8388608);
  owi_assume(b < 8388608 && b > -8388608);
  owi_assume(c < 8388608 && c > -8388608);
  owi_assume(d < 8388608 && d > -8388608);

  struct Pair *ptr2;
  A.a = a, A.b = b;
  B.a = c, B.b = d;

  pqueue_push(p2, (void *)&A);
  pqueue_top(p2, (void *)&ptr2);
  owi_assert(&A == ptr2);

  pqueue_push(p2, (void *)&B);
  pqueue_top(p2, (void *)&ptr2);

  owi_assert(((comp(&A, &B) >= 0) && ((a == ptr2->a) && (b == ptr2->b))) ||
             ((comp(&A, &B) < 0) && ((c == ptr2->a) && (d == ptr2->b))));

  teardown_tests();
  return 0;
}
