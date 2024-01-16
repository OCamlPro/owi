#ifndef _STDARG_H
#define _STDARG_H

#include <endian.h>

typedef char *va_list;
#define va_start(ap, parmn) (void)((ap) = (char *)(&(parmn) + 1))
#define va_end(ap) (void)((ap) = 0)
#define va_arg(ap, type) (((type *)((ap) = ((ap) + sizeof(type))))[-1])

#ifndef va_end
#include <stdarg-cruft.h>
#endif

#if !defined(__STRICT_ANSI__) || __STDC_VERSION__ + 0 >= 199900L
#define va_copy(d, s) __va_copy(d, s)
#endif

#endif
