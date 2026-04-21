#include <stdlib.h>
#include <owi.h>

// here, we are testing two things about malloc(0)
// 1. it may return NULL
// 2. two pointers from two distinct call to malloc should be different and thus freeing one then the other should not be a double free

void f1(void) {
  void *a = malloc(0);
  // malloc(0) returning NULL is one behaviour we should be able to model
  owi_assert(a != NULL);
}

void f2(void) {
  void *a = malloc(0);
  // malloc(0) not returning NULL is one behaviour we should be able to model
  owi_assert(a == NULL);
}


void g1(void) {
  void *a = malloc(0);
  if (a == NULL) {
    // NULL can be passed to FREE
    free(a);
    // and it can be many times
    free(a);
    owi_assert(0);
  }
}

void g2(void) {
  void *a = malloc(0);
  void *b = malloc(0);
  if (a != NULL && b != NULL) {
    free(a);
    free(b);
    // we should not have a double free error here but fail
    owi_assert(0);
  }
}

void h(void) {
  char *a = malloc(0);
  if (a != NULL) {
    // this should not be allowed
    char c = a[0];
    // this should not be read, we should fail before!
    owi_assert(c);
    owi_assert(!c);
  }
}
