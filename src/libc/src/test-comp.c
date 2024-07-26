#include <owi.h>

static __attribute__((naked,noinline,optnone)) _Bool or_(_Bool a, _Bool b) {
  __asm__ __volatile__("local.get 0;"
                       "local.get 1;"
                       "i32.or;"
                       "return;");
}

_Bool __VERIFIER_nondet_bool(void) {
  const _Bool var = owi_i32();
  const _Bool pc = or_(var == 0, var == 1);
  owi_assume(pc);
  return var;
}

char __VERIFIER_nondet_char(void) { return (char)owi_i32(); }

unsigned char __VERIFIER_nondet_uchar(void) { return (unsigned char)owi_i32(); }

short __VERIFIER_nondet_short(void) { return (short)owi_i32(); }

unsigned short __VERIFIER_nondet_ushort(void) {
  return (unsigned short)owi_i32();
}

int __VERIFIER_nondet_int(void) { return owi_i32(); }

unsigned int __VERIFIER_nondet_uint(void) { return (unsigned int)owi_i32(); }

__int128 __VERIFIER_nondet_int128(void) { return owi_i32(); }

unsigned __int128 __VERIFIER_nondet_uint128(void) {
  return (unsigned __int128)owi_i32();
}

unsigned int __VERIFIER_nondet_charp(void) {
  return (unsigned int)owi_i32();
}

long __VERIFIER_nondet_long(void) { return (long)owi_i32(); }

unsigned long __VERIFIER_nondet_ulong(void) { return (unsigned long)owi_i32(); }

long long __VERIFIER_nondet_longlong(void) { return owi_i64(); }

unsigned long long __VERIFIER_nondet_ulonglong(void) {
  return (unsigned long long)owi_i64();
}

float __VERIFIER_nondet_float(void) { return owi_f32(); }

double __VERIFIER_nondet_double(void) { return owi_f64(); }
