#ifndef STDDEF_H
#define STDDEF_H

typedef signed long ptrdiff_t;
typedef unsigned long size_t;
typedef int wchar_t;

#undef NULL
#if defined(__cplusplus)
#define NULL 0
#else
#define NULL (void *)0
#endif

#undef offsetof
#if defined(__GNUC__) && (__GNUC >= 3)
#define offsetof(type, member) __builtin_offsetof(type, member)
#else
#define offsetof(type, member) ((size_t) & ((type *)0)->member)
#endif

#endif
