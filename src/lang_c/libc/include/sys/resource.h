#ifndef _SYS_RESOURCE_H
#define _SYS_RESOURCE_H

#include <time.h>

#define RUSAGE_SELF 0
#define RUSAGE_CHILDREN (-1)
#define RUSAGE_BOTH (-2) /* sys_wait4() uses this */

struct rusage {
  struct timeval ru_utime; /* user time used */
  struct timeval ru_stime; /* system time used */
  long ru_maxrss;          /* maximum resident set size */
  long ru_ixrss;           /* integral shared memory size */
  long ru_idrss;           /* integral unshared data size */
  long ru_isrss;           /* integral unshared stack size */
  long ru_minflt;          /* page reclaims */
  long ru_majflt;          /* page faults */
  long ru_nswap;           /* swaps */
  long ru_inblock;         /* block input operations */
  long ru_oublock;         /* block output operations */
  long ru_msgsnd;          /* messages sent */
  long ru_msgrcv;          /* messages received */
  long ru_nsignals;        /* signals received */
  long ru_nvcsw;           /* voluntary context switches */
  long ru_nivcsw;          /* involuntary " */
};

struct rlimit {
  unsigned long rlim_cur;
  unsigned long rlim_max;
};

#define PRIO_MIN (-20)
#define PRIO_MAX 20

#define PRIO_PROCESS 0
#define PRIO_PGRP 1
#define PRIO_USER 2

#define RLIMIT_CPU 0   /* CPU time in ms */
#define RLIMIT_FSIZE 1 /* Maximum filesize */
#define RLIMIT_DATA 2  /* max data size */
#define RLIMIT_STACK 3 /* max stack size */
#define RLIMIT_CORE 4  /* max core file size */
#if defined(__alpha__)
#define RLIMIT_RSS 5     /* max resident set size */
#define RLIMIT_NPROC 8   /* max number of processes */
#define RLIMIT_NOFILE 6  /* max number of open files */
#define RLIMIT_MEMLOCK 9 /* max locked-in-memory address space */
#define RLIMIT_AS 7      /* address space limit */
#elif defined(__mips__)
#define RLIMIT_RSS 7     /* max resident set size */
#define RLIMIT_NPROC 8   /* max number of processes */
#define RLIMIT_NOFILE 5  /* max number of open files */
#define RLIMIT_MEMLOCK 9 /* max locked-in-memory address space */
#define RLIMIT_AS 6      /* address space limit */
#elif defined(__sparc__)
#define RLIMIT_RSS 5     /* max resident set size */
#define RLIMIT_NPROC 7   /* max number of processes */
#define RLIMIT_NOFILE 6  /* max number of open files */
#define RLIMIT_MEMLOCK 8 /* max locked-in-memory address space */
#define RLIMIT_AS 9      /* address space limit */
#else
#define RLIMIT_RSS 5     /* max resident set size */
#define RLIMIT_NPROC 6   /* max number of processes */
#define RLIMIT_NOFILE 7  /* max number of open files */
#define RLIMIT_MEMLOCK 8 /* max locked-in-memory address space */
#define RLIMIT_AS 9      /* address space limit */
#endif
#define RLIMIT_LOCKS 10      /* maximum file locks held */
#define RLIMIT_SIGPENDING 11 /* max number of pending signals */
#define RLIMIT_MSGQUEUE 12   /* maximum bytes in POSIX mqueues */
#define RLIMIT_NICE                                                            \
  13                     /* max nice prio allowed to raise to                  \
                            0-39 for nice level 19 .. -20 */
#define RLIMIT_RTPRIO 14 /* maximum realtime priority */
#define RLIMIT_RTTIME 15 /* timeout for RT tasks in us */
#define RLIM_NLIMITS 16

#define RLIM_INFINITY (~0UL)

int getpriority(int which, int who);
int setpriority(int which, int who, int prio);

int getrlimit(int resource, struct rlimit *rlim);
int getrusage(int who, struct rusage *usage);
int setrlimit(int resource, const struct rlimit *rlim);

typedef unsigned long rlim_t;

#endif
