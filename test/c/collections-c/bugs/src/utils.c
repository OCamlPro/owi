#include "utils.h"

void CHECK_EQUAL_C_STRING(char *s1, char *s2) { owi_assert(strcmp(s1, s2) == 0); }

void *copy(void *e1) {
  int *cp = (int *)malloc(sizeof(int));
  *cp = *((int *)e1);
  return cp;
}

int cmp(void const *e1, void const *e2) {
  int i = *((int *)e1);
  int j = *((int *)e2);

  if (i < j)
    return -1;
  if (i == j)
    return 0;
  return 1;
}

int zero_if_ptr_eq(void const *e1, void const *e2) { return !(e1 == e2); }
