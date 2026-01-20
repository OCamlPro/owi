#include <stdarg.h>

void exit(int);

void error(int status, int errnum, const char *format, ...) {
  (void)errnum;
  (void)format;
  if (status != 0) {
    exit(status);
  }
}
