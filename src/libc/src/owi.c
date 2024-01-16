#include <owi.h>

__attribute__((import_module("summaries"), import_name("alloc"))) void *
__owi_alloc(void *, unsigned int);
__attribute__((import_module("summaries"), import_name("dealloc"))) void
__owi_dealloc(void *);

__attribute__((import_module("symbolic"), import_name("i32_symbol"))) int
__i32();
__attribute__((import_module("symbolic"), import_name("i64_symbol"))) long long
__i64();
__attribute__((import_module("symbolic"), import_name("f32_symbol"))) float
__f32();
__attribute__((import_module("symbolic"), import_name("f64_symbol"))) double
__f64();

__attribute__((import_module("symbolic"), import_name("assume"))) void
__owi_assume(int);
__attribute__((import_module("symbolic"), import_name("assert"))) void
__owi_assert(int);

void *owi_malloc(void *base, unsigned int size) { return __owi_alloc(base, size); }
void owi_free(void *base) { __owi_dealloc(base); }

int owi_i32() { return __i32(); }
long long owi_i64() { return __i64(); }
float owi_f32() { return __f32(); }
double owi_f64() { return __f64(); }

void owi_assume(int c) { __owi_assume(c); }
void owi_assert(int c) { __owi_assert(c); }

void assume(int) __attribute__((weak, alias("owi_assume")));

int and_(int a, int b) {
  __asm__ __volatile__("local.get 0;"
                       "i32.const 0;"
                       "i32.ne;"
                       "local.get 1;"
                       "i32.const 0;"
                       "i32.ne;"
                       "i32.and;"
                       "return;");
}

int or_(int a, int b) {
  __asm__ __volatile__("local.get 0;"
                       "i32.const 0;"
                       "i32.ne;"
                       "local.get 1;"
                       "i32.const 0;"
                       "i32.ne;"
                       "i32.or;"
                       "return;");
}
