#include <owi.h>

// TODO: maybe we should do something like testing sizeof and choose the symbol size accordingly for cases like `long` that can be 32 or 64 depending on the platform ; we could also experiment by having a symbolic size for the symbols but this is probably asking for troubles...

_Bool __VERIFIER_nondet_bool(void) {
  return owi_bool();
}

char __VERIFIER_nondet_char(void) {
  return owi_char();
}

unsigned char __VERIFIER_nondet_uchar(void) {
  return owi_unsigned_char();
}

short __VERIFIER_nondet_short(void) {
  return owi_short();
}

unsigned short __VERIFIER_nondet_ushort(void) {
  return owi_unsigned_short();
}

int __VERIFIER_nondet_int(void) {
  return owi_int();
}

unsigned int __VERIFIER_nondet_uint(void) {
  return owi_unsigned_int();
}

__int128 __VERIFIER_nondet_int128(void) {
  // TODO
  return (__int128) owi_long_long();
}

unsigned __int128 __VERIFIER_nondet_uint128(void) {
  // TODO
  return (unsigned __int128) owi_long_long();
}

unsigned int __VERIFIER_nondet_charp(void) {
  return owi_unsigned_long();
}

long __VERIFIER_nondet_long(void) {
  return owi_long();
}

unsigned long __VERIFIER_nondet_ulong(void) {
  return owi_unsigned_long();
}

long long __VERIFIER_nondet_longlong(void) {
  return owi_long_long();
}

unsigned long long __VERIFIER_nondet_ulonglong(void) {
  return owi_unsigned_long_long();
}

float __VERIFIER_nondet_float(void) {
  return owi_float();
}

double __VERIFIER_nondet_double(void) {
  return owi_double();
}
