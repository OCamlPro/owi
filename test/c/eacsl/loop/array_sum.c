#include <owi.h>
#include <stdlib.h>

int main (void) {
  int n = 10;
  int *t;
  t = (int *)malloc(n * sizeof(int));

  for (int i = 0; i < n; i++) t[i] = owi_i32();

  int sum = 0;
  /*@ loop invariant 0 <= i <= n;
    @ loop invariant sum == \sum(0, i - 1, \lambda integer k; t[k]);
    @ loop variant n - i;
   */
  for (int i = 0; i < n; i++) sum += t[i];
  return 0;
}
