#ifndef _OWI_H
#define _OWI_H

void *owi_malloc(void *base, unsigned int size) __attribute__((import_module("summaries"))) __attribute__((import_name("alloc")));

void owi_free(void *) __attribute__((import_module("summaries"))) __attribute__((import_name("dealloc")));

char owi_i8(void) __attribute__((import_module("symbolic"))) __attribute__((import_name("i8_symbol")));

int owi_i32(void) __attribute__((import_module("symbolic"))) __attribute__((import_name("i32_symbol")));

long long owi_i64(void) __attribute__((import_module("symbolic"))) __attribute__((import_name("i64_symbol")));

float owi_f32(void) __attribute__((import_module("symbolic"))) __attribute__((import_name("f32_symbol")));

double owi_f64(void) __attribute__((import_module("symbolic"))) __attribute__((import_name("f64_symbol")));


void owi_assume(int c) __attribute__((import_module("symbolic"))) __attribute__((import_name("assume")));
void owi_assert(int c) __attribute__((import_module("symbolic"))) __attribute__((import_name("assert")));

#endif
