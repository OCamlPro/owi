#include <string.h>
#include <sys/types.h>

void *memccpy(void *dst, const void *src, int c, size_t count) {
  char *a = dst;
  const char *b = src;
  while (count--) {
    *a++ = *b;
    if (*b == c) {
      return (void *)a;
    }
    b++;
  }
  return 0;
}
