#ifndef _OWI_H
#define _OWI_H

void *owi_malloc(void *base, unsigned int size);

void owi_free(void *);

char owi_i8(void);

int owi_i32(void);

long long owi_i64(void);

float owi_f32(void);

double owi_f64(void);

_Bool owi_bool(void);

void owi_assume(int c);
void owi_assert(int c);

void owi_abort(void);
void owi_exit(int c);

#endif
