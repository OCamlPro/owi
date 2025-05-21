#include <string.h>

void *memcpy(void *dst, const void *src, size_t n) {
  void *res = dst;
  unsigned char *c1, *c2;
  c1 = (unsigned char *)dst;
  c2 = (unsigned char *)src;
  while (n--)
    *c1++ = *c2++;
  return (res);
}
