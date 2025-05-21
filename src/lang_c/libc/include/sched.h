#ifndef SCHED_H
#define SCHED_H

#include <sys/types.h>
#include <time.h>

/*
 * Scheduling policies
 */
#define SCHED_OTHER 0
#define SCHED_FIFO 1
#define SCHED_RR 2

/*
 * This is an additional bit set when we want to
 * yield the CPU for one re-schedule..
 */
#define SCHED_YIELD 0x10

struct sched_param {
  int sched_priority;
};

#endif
