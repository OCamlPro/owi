#ifndef STDLIB_H
#define STDLIB_H

#include <assert.h>
#include <sys/types.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

void abort(void);
void exit(int);

void *alloca(size_t);

void *calloc(size_t, size_t);
void *malloc(size_t);
void free(void *);
void *realloc(void *, size_t);

char *getenv(const char *name);
int setenv(const char *name, const char *value, int overwrite);
int unsetenv(const char *name);

int atoi(const char *nptr);

double strtod(const char *str, char **endptr);
long int strtol(const char *nptr, char **endptr, int base);
long long int strtoll(const char *nptr, char **endptr, int base);
unsigned long int strtoul(const char *nptr, char **endptr, int base);
unsigned long long int strtoull(const char *nptr, char **endptr, int base);

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *));

#endif
