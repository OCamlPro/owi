#include <string.h>

char *strchr(register const char *t, int c) {
  register char ch;

  ch = c;
  for (;;) {
    if (*t == ch)
      break;
    if (!*t)
      return 0;
    ++t;
  }
  return (char *)t;
}
