#ifndef TYPES_H
#define TYPES_H

#include <inttypes.h>
#include <stddef.h>

typedef signed long ssize_t;

typedef signed long suseconds_t; /* Used for time in microseconds. */
typedef signed long useconds_t;  /* Used for time in microseconds. */

typedef signed long long time_t;

typedef int32_t pid_t;

typedef uint32_t uid32_t;
typedef uint32_t gid32_t;

typedef int32_t clockid_t;
typedef int32_t timer_t;

typedef long int fpos_t;

typedef uint32_t socklen_t;
typedef uint16_t sa_family_t;

#endif
