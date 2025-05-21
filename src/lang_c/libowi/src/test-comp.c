#include <owi.h>

// TODO: maybe we should do something like testing sizeof and choose the symbol size accordingly for cases like `long` that can be 32 or 64 depending on the platform ; we could also experiment by having a symbolic size for the symbols but this is probably asking for troubles...

_Bool __VERIFIER_nondet_bool(void) {
  return owi_bool();
}

char __VERIFIER_nondet_char(void) {
  // TODO: could be better!
  return (char)owi_i32();
}

unsigned char __VERIFIER_nondet_uchar(void) {
  // TODO: could be better!
  return (unsigned char)owi_i32();
}

short __VERIFIER_nondet_short(void) {
  // TODO: could be better!
  return (short)owi_i32();
}

unsigned short __VERIFIER_nondet_ushort(void) {
  // TODO: could be better!
  return (unsigned short)owi_i32();
}

int __VERIFIER_nondet_int(void) { return owi_i32(); }

unsigned int __VERIFIER_nondet_uint(void) { return (unsigned int)owi_i32(); }

__int128 __VERIFIER_nondet_int128(void) {
  // TODO: looks wrong!
  return owi_i32();
}

unsigned __int128 __VERIFIER_nondet_uint128(void) {
  // TODO: looks wrong!
  return (unsigned __int128)owi_i32();
}

unsigned int __VERIFIER_nondet_charp(void) {
  // TODO: could be better!
  return (unsigned int)owi_i32();
}

long __VERIFIER_nondet_long(void) {
  // TODO: looks wrong! shouldn't it be 64?
  return (long)owi_i32();
}

unsigned long __VERIFIER_nondet_ulong(void) {
  // TODO: looks wrong!
  return (unsigned long)owi_i32();
}

long long __VERIFIER_nondet_longlong(void) { return owi_i64(); }

unsigned long long __VERIFIER_nondet_ulonglong(void) {
  return (unsigned long long)owi_i64();
}

float __VERIFIER_nondet_float(void) { return owi_f32(); }

double __VERIFIER_nondet_double(void) { return owi_f64(); }
