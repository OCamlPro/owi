#include <owi.h>

/* =============
 * basic symbols
 * ============= */

_Bool __VERIFIER_nondet_bool(void) { return owi_bool(); }

char __VERIFIER_nondet_char(void) { return owi_char(); }

unsigned char __VERIFIER_nondet_uchar(void) { return owi_unsigned_char(); }

short __VERIFIER_nondet_short(void) { return owi_short(); }

unsigned short __VERIFIER_nondet_ushort(void) { return owi_unsigned_short(); }

int __VERIFIER_nondet_int(void) { return owi_int(); }

unsigned int __VERIFIER_nondet_uint(void) { return owi_unsigned_int(); }

__int128 __VERIFIER_nondet_int128(void) { return owi_int128(); }

unsigned __int128 __VERIFIER_nondet_uint128(void) { return owi_uint128(); }

long __VERIFIER_nondet_long(void) { return owi_long(); }

unsigned long __VERIFIER_nondet_ulong(void) { return owi_unsigned_long(); }

long long __VERIFIER_nondet_longlong(void) { return owi_long_long(); }

unsigned long long __VERIFIER_nondet_ulonglong(void) {
  return owi_unsigned_long_long();
}

float __VERIFIER_nondet_float(void) { return owi_float(); }

double __VERIFIER_nondet_double(void) { return owi_double(); }

/* ====================
 * memory related stuff
 * ==================== */

unsigned int __VERIFIER_nondet_charp(void) {
  // this should return a pointer that can alias to anything
  return __VERIFIER_nondet_uint();
}

void __VERIFIER_nondet_memory(void *mem, size_t size) {
  unsigned char *p = mem;
  for (size_t i = 0; i < size; i++) {
    p[i] = __VERIFIER_nondet_uchar();
  }
}
