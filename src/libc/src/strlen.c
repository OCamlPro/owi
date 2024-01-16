#include <string.h>

size_t strlen(const char *s) {
  register size_t i;
  if (!s)
    return 0;
  for (i = 0; *s; ++s)
    ++i;
  return i;
}
