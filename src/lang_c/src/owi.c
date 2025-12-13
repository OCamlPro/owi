#include <owi.h>
#include <stdlib.h>

#define OWI_IMPORT(name)                                                       \
  __attribute__((import_module("owi"), import_name(name)))

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

#ifdef __cplusplus
bool
#else
_Bool
#endif
  owi_bool(void) {
  return magic_size_unsigned(
#ifdef __cplusplus
      bool
#else
      _Bool
#endif
  );
}

char owi_char(void) { return magic_size_signed(char); }

unsigned char owi_unsigned_char(void) {
  return magic_size_unsigned(unsigned char);
}

short owi_short(void) { return magic_size_signed(short); }

unsigned short owi_unsigned_short(void) {

  return magic_size_unsigned(unsigned short);
}

int owi_int(void) { return magic_size_signed(int); }

unsigned int owi_unsigned_int(void) {
  return magic_size_unsigned(unsigned int);
}

long owi_long(void) { return magic_size_signed(long); }

unsigned long owi_unsigned_long(void) {
  return magic_size_unsigned(unsigned long);
}

long long owi_long_long(void) { return magic_size_signed(long long); }

unsigned long long owi_unsigned_long_long(void) {
  return magic_size_unsigned(unsigned long long);
}

__int128 owi_int128(void) {
  int64_t high64 = owi_int64();
  int64_t low64 = owi_int64();

  __int128 high128 = (__int128)high64;
  __int128 low128 = (__int128)low64;

  high128 = high128 << 64;
  return high128 | low128;
}

unsigned __int128 owi_uint128(void) {
  int64_t high64 = owi_uint64();
  int64_t low64 = owi_uint64();

  unsigned __int128 high128 = (unsigned __int128)high64;
  unsigned __int128 low128 = (unsigned __int128)low64;

  high128 = high128 << 64;
  return high128 | low128;
}

/* stdlib redifinitions */

OWI_IMPORT("abort") __attribute__((noreturn)) void abort(void);

OWI_IMPORT("exit") __attribute__((noreturn)) void exit(int);

extern unsigned char __heap_base;
unsigned int bump_pointer = &__heap_base;

void *malloc(size_t size) {
  if (size == 0) {
    // TODO: this could also be a proper *unique* pointer
    // that is, calling malloc(0) two times, and freeing the two pointers should
    // not lead to a double free see issue# 602 and the test added in #604 whose
    // result should change once this is added
    return NULL;
  }
  unsigned int start = bump_pointer;
  unsigned int closest_pow2 =
      1 << (sizeof(size_t) * 8 - (__builtin_clz(size) + 1));
  unsigned int align = (closest_pow2 <= 16) ? closest_pow2 : 16;
  unsigned int off_align = bump_pointer % align;

  if (off_align != 0) {
    start += (align - off_align);
  }
  bump_pointer = size + start;
  return (void *)owi_malloc(start, size);
}

OWI_IMPORT("free") void free(void *ptr);
