#ifndef ASSERT_H
#define ASSERT_H

#define static_assert(...)

# if defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L
#  define __ASSERT_FUNCTION    __func__
# else
#  define __ASSERT_FUNCTION    (__func__ ? __func__ : "(unknown)")
# endif


extern void __assert_fail(const char *__assertion, const char *__file,
                          unsigned int __line, const char *__function);
#define assert(expr)                                                           \
  ((void)((expr)                                                               \
              ? 0                                                              \
              : (__assert_fail(#expr, __FILE__, __LINE__, __ASSERT_FUNCTION),  \
                 0)))

#endif
