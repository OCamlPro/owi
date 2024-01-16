#include <sys/resource.h>

int getrlimit(int resource, struct rlimit *rlim) { return 0; }
int getrusage(int who, struct rusage *usage) { return 0; }
int setrlimit(int resource, const struct rlimit *rlim) { return 0; }
