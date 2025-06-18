#include <owi.h>

#define magic_size_signed(T) \
  ({ \
    _Static_assert(sizeof(T) == 1 || sizeof(T) == 2 || sizeof(T) == 4 || sizeof(T) == 8, "Unsupported type size!"); \
    (sizeof(T) == 1) ? (T)(owi_int8()) : \
    (sizeof(T) == 2) ? (T)(owi_int16()) : \
    (sizeof(T) == 4) ? (T)(owi_int32()) : \
    (T)(owi_int64()); \
  })

#define magic_size_unsigned(T) \
  ({ \
    _Static_assert(sizeof(T) == 1 || sizeof(T) == 2 || sizeof(T) == 4 || sizeof(T) == 8, "Unsupported type size!"); \
    (sizeof(T) == 1) ? (T)(owi_uint8()) : \
    (sizeof(T) == 2) ? (T)(owi_uint16()) : \
    (sizeof(T) == 4) ? (T)(owi_uint32()) : \
    (T)(owi_uint64()); \
  })

char owi_char(void) {
  return magic_size_signed(char);
}

unsigned char owi_unsigned_char(void) {
  return magic_size_unsigned(unsigned char);
}

short owi_short(void) {
  return magic_size_signed(short);
}

unsigned short owi_unsigned_short(void) {
  return magic_size_unsigned(unsigned short);
}

int owi_int(void) {
  return magic_size_signed(int);
}

unsigned int owi_unsigned_int(void) {
  return magic_size_unsigned(unsigned int);
}

long owi_long(void) {
  return magic_size_signed(long);
}

unsigned long owi_unsigned_long(void) {
  return magic_size_unsigned(unsigned long);
}

long long owi_long_long(void) {
  return magic_size_signed(long long);
}

unsigned long long owi_unsigned_long_long(void) {
  return magic_size_unsigned(unsigned long long);
}
