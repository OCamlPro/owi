#include <string.h>
#include <sys/types.h>

void *memmove(void *dst, const void *src, size_t count) {
  char *a = dst;
  const char *b = src;
  if (src != dst) {
    if (src > dst) {
      while (count--)
        *a++ = *b++;
    } else {
      a += count - 1;
      b += count - 1;
      while (count--)
        *a-- = *b--;
    }
  }
  return dst;
}
