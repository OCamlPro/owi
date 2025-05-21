#include <string.h>
#include <sys/types.h>

/* gcc is broken and has a non-SUSv2 compliant internal prototype.
 * This causes it to warn about a type mismatch here.  Ignore it. */
int memcmp(const void *dst, const void *src, size_t count) {
  register int r;
  register const unsigned char *d = dst;
  register const unsigned char *s = src;
  ++count;
  while (--count) {
    // TODO: check this change !
    r = (*d - *s);
    if (r) {
      return r;
    }
    ++d;
    ++s;
  }
  return 0;
}
