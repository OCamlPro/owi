#ifndef _OWI_H
#define _OWI_H

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

__attribute__((import_module("symbolic"), import_name("i8_symbol")))   int8_t   owi_int8(void);
__attribute__((import_module("symbolic"), import_name("i8_symbol")))  uint8_t  owi_uint8(void);
__attribute__((import_module("symbolic"), import_name("i16_symbol")))  int16_t  owi_int16(void);
__attribute__((import_module("symbolic"), import_name("i16_symbol"))) uint16_t owi_uint16(void);
__attribute__((import_module("symbolic"), import_name("i32_symbol")))  int32_t  owi_int32(void);
__attribute__((import_module("symbolic"), import_name("i32_symbol"))) uint32_t owi_uint32(void);
__attribute__((import_module("symbolic"), import_name("i64_symbol")))  int64_t  owi_int64(void);
__attribute__((import_module("symbolic"), import_name("i64_symbol"))) uint64_t owi_uint64(void);

#ifndef __FRAMAC__
__attribute__((import_module("symbolic"), import_name("v128_symbol"))) __int128  owi_int128(void);
__attribute__((import_module("symbolic"), import_name("v128_symbol"))) unsigned __int128 owi_uint128(void);
#endif

_Static_assert(sizeof(float) == 4, "Unsupported float size. Please open an issue.");
__attribute__((import_module("symbolic"), import_name("f32_symbol"))) float owi_float(void);

_Static_assert(sizeof(double) == 8, "Unsupported double size. Please open an issue.");
__attribute__((import_module("symbolic"), import_name("f64_symbol"))) double owi_double(void);

// TODO
// long double owi_long_double(void);

__attribute__((import_module("symbolic"), import_name("bool_symbol")))
#ifdef __cplusplus
  bool
#else
  _Bool
#endif
owi_bool(void);

char owi_char(void);
unsigned char owi_unsigned_char(void);
short owi_short(void);
unsigned short owi_unsigned_short(void);
int owi_int(void);
unsigned int owi_unsigned_int(void);
long owi_long(void);
unsigned long owi_unsigned_long(void);
long long owi_long_long(void);
unsigned long long owi_unsigned_long_long(void);

// TODO: improve this
__attribute__((import_module("symbolic"), import_name("range_symbol"))) int owi_range(int lo, int hi);

__attribute__((import_module("summaries"), import_name("alloc"))) void *owi_malloc(void *, unsigned int);
__attribute__((import_module("symbolic"), import_name("print_char"))) void owi_print_char(int);

__attribute__((import_module("symbolic"), import_name("cov_label_is_covered"))) void owi_label_is_covered(int id);
__attribute__((import_module("symbolic"), import_name("cov_label_set"))) void owi_label_set(int id, char * name);

#ifdef __OWI_INTERNALS

__attribute__((import_module("symbolic"), import_name("invisible_bool_symbol"))) int owi_invisible_bool(void);
__attribute__((import_module("symbolic"), import_name("open_scope"))) void owi_open_scope(const char *name);
__attribute__((import_module("symbolic"), import_name("close_scope"))) void owi_close_scope(void);

#endif

__attribute__((import_module("symbolic"), import_name("assume"))) void owi_assume(int);
__attribute__((import_module("symbolic"), import_name("assert"))) void owi_assert(int);
// TODO: do we need these two ? we already have abort and exit in stdlib that are exactly the same
__attribute__((import_module("summaries"), import_name("abort"))) void abort(void);
__attribute__((import_module("summaries"), import_name("exit"))) void exit(int);

#ifdef __cplusplus
}
#endif

#endif
