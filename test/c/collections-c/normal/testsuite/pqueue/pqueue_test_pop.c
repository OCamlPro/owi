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

  int x = owi_i32();
  int y = owi_i32();
  int z = owi_i32();
  int *ptr;

  owi_assume(x < 8388608 && x > -8388608);
  owi_assume(y < 8388608 && y > -8388608);
  owi_assume(z < 8388608 && z > -8388608);

  owi_assume(x > z && z > y);

  pqueue_push(p1, (void *)&y);
  pqueue_push(p1, (void *)&x);
  pqueue_push(p1, (void *)&z);

  pqueue_pop(p1, (void *)&ptr);
  owi_assert(&x == ptr);

  pqueue_pop(p1, (void *)&ptr);
  owi_assert(&z == ptr);

  pqueue_pop(p1, (void *)&ptr);
  owi_assert(&y == ptr);

  struct Pair *ptr2;
  A.a = a, A.b = b;
  B.a = c, B.b = d;
  C.a = e, C.b = f;

  owi_assume(comp(&C, &A) > 0 && comp(&A, &B) > 0);

  pqueue_push(p2, (void *)&A);
  pqueue_push(p2, (void *)&B);
  pqueue_push(p2, (void *)&C);

  pqueue_pop(p2, (void *)&ptr2);
  owi_assert(&C == ptr2);

  pqueue_pop(p2, (void *)&ptr2);
  owi_assert(&A == ptr2);

  pqueue_pop(p2, (void *)&ptr2);
  owi_assert(&B == ptr2);

  teardown_tests();
  return 0;
}
