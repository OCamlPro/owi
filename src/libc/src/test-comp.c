#include <owi.h>

_Bool __VERIFIER_nondet_bool() {
  _Bool var = owi_i32();
  owi_assume(or_(var == 0, var == 1));
  return var;
}

char __VERIFIER_nondet_char() { return (char)owi_i32(); }

unsigned char __VERIFIER_nondet_uchar() { return (unsigned char)owi_i32(); }

short __VERIFIER_nondet_short() { return (short)owi_i32(); }

unsigned short __VERIFIER_nondet_ushort() {
  return (unsigned short)owi_i32();
}

int __VERIFIER_nondet_int() { return owi_i32(); }

unsigned int __VERIFIER_nondet_uint() { return (unsigned int)owi_i32(); }

__int128 __VERIFIER_nondet_int128() { return owi_i32(); }

unsigned __int128 __VERIFIER_nondet_uint128() {
  return (unsigned __int128)owi_i32();
}

unsigned int __VERIFIER_nondet_charp() {
  return (unsigned int)owi_i32();
}

long __VERIFIER_nondet_long() { return (long)owi_i32(); }

unsigned long __VERIFIER_nondet_ulong() { return (unsigned long)owi_i32(); }

long long __VERIFIER_nondet_longlong() { return owi_i64(); }

unsigned long long __VERIFIER_nondet_ulonglong() {
  return (unsigned long long)owi_i64();
}

float __VERIFIER_nondet_float() { return owi_f32(); }

double __VERIFIER_nondet_double() { return owi_f64(); }
