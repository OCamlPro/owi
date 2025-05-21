#include <string.h>

char *strrchr(const char *t, int c) {
  register char ch;
  register const char *l = 0;

  ch = c;
  for (;;) {
    if (*t == ch)
      l = t;
    if (!*t)
      return (char *)l;
    ++t;
  }
  return (char *)l;
}
