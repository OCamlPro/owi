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
  char str_a[] = {a, '\0'};

  int b = owi_i32();
  char str_b[] = {b, '\0'};

  int c = owi_i32();
  char str_c[] = {c, '\0'};

  int d = owi_i32();
  char str_d[] = {d, '\0'};

  int e = owi_i32();
  char str_e[] = {e, '\0'};

  int f = owi_i32();
  char str_f[] = {f, '\0'};

  int g = owi_i32();
  char str_g[] = {g, '\0'};

  owi_assume(b != a && b != c && b != d);

  deque_add(deque, str_a);
  deque_add(deque, str_b);
  deque_add(deque, str_c);
  deque_add(deque, str_d);

  Deque *d2;
  deque_new(&d2);

  deque_add(d2, str_e);
  deque_add(d2, str_f);
  deque_add(d2, str_g);

  DequeZipIter zip;
  deque_zip_iter_init(&zip, deque, d2);

  size_t i = 0;

  void *e1, *e2;
  while (deque_zip_iter_next(&zip, &e1, &e2) != CC_ITER_END) {
    if (i == 0) {
      owi_assert(strcmp(str_a, (char *)e1) == 0);
      owi_assert(strcmp(str_e, (char *)e2) == 0);
    }
    if (i == 2) {
      owi_assert(strcmp(str_c, (char *)e1) == 0);
      owi_assert(strcmp(str_g, (char *)e2) == 0);
    }
    i++;
  }
  owi_assert(3 == i);
  deque_destroy(d2);

  teardown_tests();
  return 0;
}
