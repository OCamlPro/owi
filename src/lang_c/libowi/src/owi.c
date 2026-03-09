#include <owi.h>

#define magic_size_signed(T)                                                   \
  ({                                                                           \
    _Static_assert(sizeof(T) == 1 || sizeof(T) == 2 || sizeof(T) == 4 ||       \
                       sizeof(T) == 8,                                         \
                   "Unsupported type size!");                                  \
    (sizeof(T) == 1)   ? (T)(owi_int32())                                      \
    : (sizeof(T) == 2) ? (T)(owi_int32())                                      \
    : (sizeof(T) == 4) ? (T)(owi_int32())                                      \
                       : (T)(owi_int64());                                     \
  })

#define magic_size_unsigned(T)                                                 \
  ({                                                                           \
    _Static_assert(sizeof(T) == 1 || sizeof(T) == 2 || sizeof(T) == 4 ||       \
                       sizeof(T) == 8,                                         \
                   "Unsupported type size!");                                  \
    (sizeof(T) == 1)   ? (T)(owi_uint32())                                     \
    : (sizeof(T) == 2) ? (T)(owi_uint32())                                     \
    : (sizeof(T) == 4) ? (T)(owi_uint32())                                     \
                       : (T)(owi_uint64());                                    \
  })

#define OWI_NAMED(type, name)                                                  \
  type owi_named_##name(const char *n) {                                       \
    owi_open_scope(n);                                                         \
    type sym = owi_##name();                                                   \
    owi_close_scope();                                                         \
    return sym;                                                                \
  }

OWI_NAMED(int32_t, int32);
OWI_NAMED(uint32_t, uint32);
OWI_NAMED(int64_t, int64);
OWI_NAMED(uint64_t, uint64);
OWI_NAMED(float, float);
OWI_NAMED(double, double);
int owi_range_named(const char *name, int lo, int hi) {
  owi_open_scope(name);
  int sym = owi_range(lo, hi);
  owi_close_scope();
  return sym;
};

#ifdef __cplusplus
#define OWI_BOOL bool
#else
#define OWI_BOOL _Bool
#endif

OWI_BOOL owi_bool(void) { return magic_size_unsigned(OWI_BOOL); }
OWI_NAMED(OWI_BOOL, bool)

char owi_char(void) { return magic_size_signed(char); }
OWI_NAMED(char, char)

unsigned char owi_unsigned_char(void) {
  return magic_size_unsigned(unsigned char);
}
OWI_NAMED(unsigned char, unsigned_char)

short owi_short(void) { return magic_size_signed(short); }
OWI_NAMED(short, short)

unsigned short owi_unsigned_short(void) {
  return magic_size_unsigned(unsigned short);
}
OWI_NAMED(unsigned short, unsigned_short)

int owi_int(void) { return magic_size_signed(int); }
OWI_NAMED(int, int)

unsigned int owi_unsigned_int(void) {
  return magic_size_unsigned(unsigned int);
}
OWI_NAMED(unsigned int, unsigned_int)

long owi_long(void) { return magic_size_signed(long); }
OWI_NAMED(long, long)

unsigned long owi_unsigned_long(void) {
  return magic_size_unsigned(unsigned long);
}
OWI_NAMED(unsigned long, unsigned_long)

long long owi_long_long(void) { return magic_size_signed(long long); }
OWI_NAMED(long long, long_long)

unsigned long long owi_unsigned_long_long(void) {
  return magic_size_unsigned(unsigned long long);
}
OWI_NAMED(unsigned long long, unsigned_long_long)

__int128 owi_int128(void) {
  int64_t high64 = owi_int64();
  int64_t low64 = owi_int64();

  __int128 high128 = (__int128)high64;
  __int128 low128 = (__int128)low64;

  high128 = high128 << 64;
  return high128 | low128;
}
OWI_NAMED(__int128, int128)

unsigned __int128 owi_uint128(void) {
  int64_t high64 = owi_uint64();
  int64_t low64 = owi_uint64();

  unsigned __int128 high128 = (unsigned __int128)high64;
  unsigned __int128 low128 = (unsigned __int128)low64;

  high128 = high128 << 64;
  return high128 | low128;
}
OWI_NAMED(unsigned __int128, uint128)
