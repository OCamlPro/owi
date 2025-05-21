#include <sys/types.h>
#include <string.h>

/* gcc is broken and has a non-SUSv2 compliant internal prototype.
 * This causes it to warn about a type mismatch here.  Ignore it. */
char *strncpy(char *dest, const char *src, size_t n) {
  memccpy(dest,src,0,n);
  return dest;
}
