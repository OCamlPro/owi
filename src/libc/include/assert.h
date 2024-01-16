#ifndef ASSERT_H
#define ASSERT_H

extern void __assert_fail(const char *__assertion, const char *__file,
                          unsigned int __line, const char *__function);
#define assert(expr)                                                           \
  ((void)((expr)                                                               \
              ? 0                                                              \
              : (__assert_fail(#expr, __FILE__, __LINE__, __ASSERT_FUNCTION),  \
                 0)))

#endif
