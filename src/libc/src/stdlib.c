#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <owi.h>
#include <stdlib.h>

#define ABS_LONG_MIN 2147483648UL

#ifndef WANT_STRTOD_WITHOUT_LONG_DOUBLE
#define ldbltype long double
#else
#define ldbltype double
#endif

__attribute__((import_module("summaries"), import_name("abort"))) void
owi_abort(void);
__attribute__((import_module("summaries"), import_name("exit"))) void
owi_exit(int);

void abort(void) { owi_abort(); }
void exit(int status) { owi_exit(status); }

extern unsigned char __heap_base;
unsigned int bump_pointer = &__heap_base;

void *malloc(size_t size) {
  unsigned int start;
  unsigned int closest_pow2 = 1 << (sizeof(size_t)*8 - (__builtin_clz(size) + 1));
  unsigned int align = (closest_pow2 <= 16) ? closest_pow2 : 16;
  unsigned int off_align = bump_pointer % align;
  if ( off_align == 0 ) {
    start = bump_pointer;
  }
  else {
    start = bump_pointer + (align - off_align);
  }
  bump_pointer = size + start;
  return (void *)owi_malloc(start, size);
}

void *alloca(size_t size) { return malloc(size); }

void *calloc(size_t nmemb, size_t size) {
  // TODO: correctly handle overflow on multiplication
  return malloc(nmemb * size);
}

void *realloc(void *ptr, size_t size) {
  // TODO: fix
  owi_free(ptr);
  return (void *)owi_malloc(ptr, size);
}

void free(void *ptr) { owi_free(ptr); }

char *getenv(const char *name) { return (char *)0; }
int setenv(const char *name, const char *value, int overwrite) { return 0; }
int unsetenv(const char *name) { return 0; }

int atoi(const char *s) {
  long int v = 0;
  int sign = 1;
  while (*s == ' ' || (unsigned int)(*s - 9) < 5u)
    s++;
  switch (*s) {
  case '-':
    sign = -1; /* fall through */
  case '+':
    ++s;
  }
  while ((unsigned int)(*s - '0') < 10u) {
    v = v * 10 + *s - '0';
    ++s;
  }
  return sign == -1 ? -v : v;
}

double strtod(const char *s, char **endptr) {
  register const char *p = s;
  register ldbltype value = 0.;
  int sign = +1;
  ldbltype factor;
  unsigned int expo;

  while (isspace(*p))
    p++;

  switch (*p) {
  case '-':
    sign = -1; /* fall through */
  case '+':
    p++;
  default:
    break;
  }

  while ((unsigned int)(*p - '0') < 10u)
    value = value * 10 + (*p++ - '0');

  if (*p == '.') {
    factor = 1.;

    p++;
    while ((unsigned int)(*p - '0') < 10u) {
      factor *= 0.1;
      value += (*p++ - '0') * factor;
    }
  }

  if ((*p | 32) == 'e') {
    expo = 0;
    factor = 10.;

    switch (*++p) { // ja hier weiß ich nicht, was mindestens nach einem 'E'
                    // folgenden MUSS.
    case '-':
      factor = 0.1; /* fall through */
    case '+':
      p++;
      break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      break;
    default:
      value = 0.;
      p = s;
      goto done;
    }

    while ((unsigned int)(*p - '0') < 10u)
      expo = 10 * expo + (*p++ - '0');

    while (1) {
      if (expo & 1)
        value *= factor;
      if ((expo >>= 1) == 0)
        break;
      factor *= factor;
    }
  }

done:
  if (endptr != NULL)
    *endptr = (char *)p;

  return value * sign;
}

long int strtol(const char *nptr, char **endptr, int base) {
  int neg = 0;
  unsigned long int v;
  const char *orig = nptr;

  while (isspace(*nptr))
    nptr++;

  if (*nptr == '-' && isalnum(nptr[1])) {
    neg = -1;
    ++nptr;
  }
  v = strtoul(nptr, endptr, base);
  if (endptr && *endptr == nptr)
    *endptr = (char *)orig;
  if (v >= ABS_LONG_MIN) {
    if (v == ABS_LONG_MIN && neg) {
      errno = 0;
      return v;
    }
    errno = ERANGE;
    return (neg ? LONG_MIN : LONG_MAX);
  }
  return (neg ? -v : v);
}

long long int strtoll(const char *nptr, char **endptr, int base) {
  int neg = 0;
  unsigned long long int v;
  const char *orig = nptr;

  while (isspace(*nptr))
    nptr++;

  if (*nptr == '-' && isalnum(nptr[1])) {
    neg = -1;
    nptr++;
  }
  v = strtoull(nptr, endptr, base);
  if (endptr && *endptr == nptr)
    *endptr = (char *)orig;
  if (v > LLONG_MAX) {
    if (v == 0x8000000000000000ull && neg) {
      errno = 0;
      return v;
    }
    errno = ERANGE;
    return (neg ? LLONG_MIN : LLONG_MAX);
  }
  return (neg ? -v : v);
}

unsigned long int strtoul(const char *ptr, char **endptr, int base) {
  int neg = 0, overflow = 0;
  unsigned long int v = 0;
  const char *orig;
  const char *nptr = ptr;

  while (isspace(*nptr))
    ++nptr;

  if (*nptr == '-') {
    neg = 1;
    nptr++;
  } else if (*nptr == '+')
    ++nptr;
  orig = nptr;
  if (base == 16 && nptr[0] == '0')
    goto skip0x;
  if (base) {
    register unsigned int b = base - 2;
    if (b > 34) {
      errno = EINVAL;
      return 0;
    }
  } else {
    if (*nptr == '0') {
      base = 8;
    skip0x:
      if ((nptr[1] == 'x' || nptr[1] == 'X') && isxdigit(nptr[2])) {
        nptr += 2;
        base = 16;
      }
    } else
      base = 10;
  }
  while (*nptr) {
    register unsigned char c = *nptr;
    c = (c >= 'a'   ? c - 'a' + 10
         : c >= 'A' ? c - 'A' + 10
         : c <= '9' ? c - '0'
                    : 0xff);
    if (c >= base)
      break; /* out of base */
    {
      register unsigned long x = (v & 0xff) * base + c;
      register unsigned long w = (v >> 8) * base + (x >> 8);
      if (w > (ULONG_MAX >> 8))
        overflow = 1;
      v = (w << 8) + (x & 0xff);
    }
    ++nptr;
  }
  if (nptr == orig) { /* no conversion done */
    nptr = ptr;
    errno = EINVAL;
    v = 0;
  }
  if (endptr)
    *endptr = (char *)nptr;
  if (overflow) {
    errno = ERANGE;
    return ULONG_MAX;
  }
  return (neg ? -v : v);
}

unsigned long long int strtoull(const char *ptr, char **endptr, int base) {
  int neg = 0, overflow = 0;
  long long int v = 0;
  const char *orig;
  const char *nptr = ptr;

  while (isspace(*nptr))
    ++nptr;

  if (*nptr == '-') {
    neg = 1;
    nptr++;
  } else if (*nptr == '+')
    ++nptr;
  orig = nptr;
  if (base == 16 && nptr[0] == '0')
    goto skip0x;
  if (base) {
    register unsigned int b = base - 2;
    if ((b > 34)) {
      errno = EINVAL;
      return 0;
    }
  } else {
    if (*nptr == '0') {
      base = 8;
    skip0x:
      if (((*(nptr + 1) == 'x') || (*(nptr + 1) == 'X')) && isxdigit(nptr[2])) {
        nptr += 2;
        base = 16;
      }
    } else
      base = 10;
  }
  while ((*nptr)) {
    register unsigned char c = *nptr;
    c = (c >= 'a'   ? c - 'a' + 10
         : c >= 'A' ? c - 'A' + 10
         : c <= '9' ? c - '0'
                    : 0xff);
    if ((c >= base))
      break; /* out of base */
    {
      register unsigned long x = (v & 0xff) * base + c;
      register unsigned long long w = (v >> 8) * base + (x >> 8);
      if (w > (ULLONG_MAX >> 8))
        overflow = 1;
      v = (w << 8) + (x & 0xff);
    }
    ++nptr;
  }
  if (nptr == orig) { /* no conversion done */
    nptr = ptr;
    errno = EINVAL;
    v = 0;
  }
  if (endptr)
    *endptr = (char *)nptr;
  if (overflow) {
    errno = ERANGE;
    return ULLONG_MAX;
  }
  return (neg ? -v : v);
}

static void exch(char *base, size_t size, size_t a, size_t b) {
  char *x = base + a * size;
  char *y = base + b * size;
  while (size) {
    char z = *x;
    *x = *y;
    *y = z;
    --size;
    ++x;
    ++y;
  }
}

/* Quicksort with 3-way partitioning, ala Sedgewick */
/* Blame him for the scary variable names */
/* http://www.cs.princeton.edu/~rs/talks/QuicksortIsOptimal.pdf */
static void quicksort(char *base, size_t size, ssize_t l, ssize_t r,
                      int (*compar)(const void *, const void *)) {
  ssize_t i = l - 1, j = r, p = l - 1, q = r, k;
  char *v = base + r * size;
  if (r <= l)
    return;
  /*
     We chose the rightmost element in the array to be sorted as pivot,
     which is OK if the data is random, but which is horrible if the
     data is already sorted.  Try to improve by chosing the middle
     element instead.
   */
  exch(base, size, l + (r - l) / 2, r);

  for (;;) {
    while (++i != r && compar(base + i * size, v) < 0)
      ;
    while (compar(v, base + (--j) * size) < 0)
      if (j == l)
        break;
    if (i >= j)
      break;
    exch(base, size, i, j);
    if (compar(base + i * size, v) == 0)
      exch(base, size, ++p, i);
    if (compar(v, base + j * size) == 0)
      exch(base, size, j, --q);
  }
  exch(base, size, i, r);
  j = i - 1;
  ++i;
  for (k = l; k < p; k++, j--)
    exch(base, size, k, j);
  for (k = r - 1; k > q; k--, i++)
    exch(base, size, i, k);
  quicksort(base, size, l, j, compar);
  quicksort(base, size, i, r, compar);
}

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *)) {
  /* check for integer overflows */
  if (nmemb >= (((size_t)-1) >> 1) || size >= (((size_t)-1) >> 1))
    return;
#if 0
  if (sizeof(size_t) < sizeof(unsigned long long)) {
    if ((unsigned long long)size * nmemb > (size_t)-1) return;
  } else {
    if (size*nmemb/nmemb != size) return;
  }
#endif
  if (nmemb > 1)
    quicksort(base, size, 0, nmemb - 1, compar);
}

int posix_memalign(void **memptr, size_t alignment, size_t size) { return 0; }

// TODO: OWI external
char getchar(void) { return ' '; }
