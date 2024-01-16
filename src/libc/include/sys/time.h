#ifndef SYS_TIME_H
#define SYS_TIME_H

#include <sys/types.h>

struct timespec {
  time_t tv_sec;	/* seconds */
  long tv_nsec;		/* nanoseconds */
};

struct timeval {
  time_t tv_sec;	/* seconds */
  suseconds_t tv_usec;	/* microseconds */
};

struct timezone {
  int tz_minuteswest;	/* minutes west of Greenwich */
  int tz_dsttime;	/* type of dst correction */
};

#define	ITIMER_REAL	0
#define	ITIMER_VIRTUAL	1
#define	ITIMER_PROF	2

struct itimerspec {
  struct timespec it_interval;	/* timer period */
  struct timespec it_value;	/* timer expiration */
};

struct itimerval {
  struct timeval it_interval;	/* timer interval */
  struct timeval it_value;	/* current value */
};

typedef struct timezone *__timezone_ptr_t;

struct tm {
  int tm_sec;			/* Seconds.	[0-60] (1 leap second) */
  int tm_min;			/* Minutes.	[0-59] */
  int tm_hour;			/* Hours.	[0-23] */
  int tm_mday;			/* Day.		[1-31] */
  int tm_mon;			/* Month.	[0-11] */
  int tm_year;			/* Year - 1900. */
  int tm_wday;			/* Day of week.	[0-6] */
  int tm_yday;			/* Days in year.[0-365]	*/
  int tm_isdst;			/* DST.		[-1/0/1]*/

  long int tm_gmtoff;		/* Seconds east of UTC.  */
  const char *tm_zone;		/* Timezone abbreviation.  */
};

#endif
