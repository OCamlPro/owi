#include "owi.h"
#include "stack.h"

static Stack *s;

void setup_tests() { stack_new(&s); }

void teardown_tests() { stack_destroy(s); }

int main() {
  setup_tests();

  int a = owi_i32();
  int b = owi_i32();
  int c = owi_i32();

  stack_push(s, (void *)&a);
  stack_push(s, (void *)&b);
  stack_push(s, (void *)&c);

  int *pop;
  int *peek;

  stack_pop(s, (void *)&pop);
  owi_assert(&c == pop);

  stack_peek(s, (void *)&peek);
  owi_assert(&b == peek);

  teardown_tests();
  return 0;
}
