#ifndef _OWI_H
#define _OWI_H

void *owi_malloc(void *base, unsigned int size);
void owi_free(void *);

int owi_i32(void);
long long owi_i64(void);
float owi_f32(void);
double owi_f64(void);

void owi_assume(int c);
void owi_assert(int c);

int and_(int, int);
int or_(int, int);

#endif
