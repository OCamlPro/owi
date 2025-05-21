#ifndef STRING_H
#define STRING_H

#include <stddef.h>
#include <sys/cdefs.h>
#include <sys/types.h>

char *strcpy(char *, const char *);

void *memccpy(void *__restrict__ dest, const void *__restrict__ src, int c,
              size_t n);
void *memmove(void *dest, const void *src, size_t n);

void *memset(void *, int, size_t);
int memcmp(const void *s1, const void *s2, size_t n);
void *memcpy(void *, const void *, size_t);

char *strncpy(char *, const char *, size_t);
int strncmp(const char *s1, const char *s2, size_t n);

int strcmp(const char *s1, const char *s2);

size_t strlen(const char *s);

char *strstr(const char *hatstack, const char *needle);

char *strdup(const char *s);

char *strchr(const char *str, int c);
char *strrchr(const char *str, int c);

void *memchr(const void *s, int c, size_t n);

char *strerror(int errnum);

#endif
