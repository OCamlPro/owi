#ifndef _OWI_H
#define _OWI_H

#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

__attribute__((import_module("owi"), import_name("i32_symbol")))  int32_t  owi_int32(void);
int32_t owi_named_int32(const char* name);
__attribute__((import_module("owi"), import_name("i32_symbol"))) uint32_t owi_uint32(void);
uint32_t owi_named_uint32(const char* name);
__attribute__((import_module("owi"), import_name("i64_symbol")))  int64_t  owi_int64(void);
int64_t owi_named_int64(const char* name);
__attribute__((import_module("owi"), import_name("i64_symbol"))) uint64_t owi_uint64(void);
uint64_t owi_named_uint64(const char* name);

#ifndef __FRAMAC__
__int128 owi_int128(void);
__int128 owi_named_int128(const char* name);
unsigned __int128 owi_uint128(void);
unsigned __int128 owi_named_uint128(const char* name);
#endif

_Static_assert(sizeof(float) == 4, "Unsupported float size. Please open an issue.");
__attribute__((import_module("owi"), import_name("f32_symbol"))) float owi_float(void);
float owi_named_float(const char* name);

_Static_assert(sizeof(double) == 8, "Unsupported double size. Please open an issue.");
__attribute__((import_module("owi"), import_name("f64_symbol"))) double owi_double(void);
double owi_named_double(const char* name);

// TODO
// long double owi_long_double(void);


#ifdef __cplusplus
  bool
#else
  _Bool
#endif
     owi_bool(void);
#ifdef __cplusplus
  bool
#else
  _Bool
#endif
     owi_named_bool(const char* name);
char owi_char(void);
char owi_named_char(const char* name);
unsigned char owi_unsigned_char(void);
unsigned char owi_named_unsigned_char(const char* name);
short owi_short(void);
short owi_named_short(const char* name);
unsigned short owi_unsigned_short(void);
unsigned short owi_named_unsigned_short(const char* name);
int owi_int(void);
int owi_named_int(const char* name);
unsigned int owi_unsigned_int(void);
unsigned int owi_named_unsigned_int(const char* name);
long owi_long(void);
long owi_named_long(const char* name);
unsigned long owi_unsigned_long(void);
unsigned long owi_named_unsigned_long(const char* name);
long long owi_long_long(void);
long long owi_named_long_long(const char* name);
unsigned long long owi_unsigned_long_long(void);
unsigned long long owi_named_unsigned_long_long(const char* name);

// TODO: improve this
__attribute__((import_module("owi"), import_name("range_symbol"))) int owi_range(int lo, int hi);
int owi_named_range(const char* name, int lo, int hi);

__attribute__((import_module("owi"), import_name("alloc"))) void *owi_malloc(void *, unsigned int);
__attribute__((import_module("owi"), import_name("print_char"))) void owi_print_char(int);

__attribute__((import_module("owi"), import_name("cov_label_is_covered"))) int owi_label_is_covered(int id);
__attribute__((import_module("owi"), import_name("cov_label_set"))) void owi_label_set(int id, char * name);

#ifdef __OWI_INTERNALS

__attribute__((import_module("owi"), import_name("invisible_bool_symbol"))) int owi_invisible_bool(void);

#endif

__attribute__((import_module("owi"), import_name("open_scope_null_terminated"))) void owi_open_scope(const char *name);
__attribute__((import_module("owi"), import_name("close_scope"))) void owi_close_scope(void);

__attribute__((import_module("owi"), import_name("assume"))) void owi_assume(int);
__attribute__((import_module("owi"), import_name("assert"))) void owi_assert(int);
// TODO: do we need these two ? we already have abort and exit in stdlib that are exactly the same
__attribute__((import_module("owi"), import_name("abort"))) void abort(void);
__attribute__((import_module("owi"), import_name("exit"))) void exit(int);

#ifdef __cplusplus
}
#endif

#endif
