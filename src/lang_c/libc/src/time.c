#include <time.h>

int clock_gettime(clockid_t clock_id, struct timespec *tp) { return 0; }

time_t time(time_t *t) { return 0; }
time_t timegm(struct tm *timeptr) { return 0; }
time_t mktime(struct tm *timeptr) { return 0; }

struct tm *localtime_r(const time_t *t, struct tm *r) { return (struct tm *)0; }
struct tm *gmtime_r(const time_t *t, struct tm *r) { return (struct tm *)0; }

int nanosleep(const struct timespec *rqtp, struct timespec *rmtp) { return 0; }

char *ctime(const time_t *timep) { return 0; }
