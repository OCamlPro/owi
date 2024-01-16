#ifndef UNISTD_H
#define UNISTD_H

#include <sys/types.h>

extern int optind, opterr, optopt;
extern char *optarg;
/* int getopt(int argc, char *const argv[], const char *options); */

/* int open(const char* pathname,int flags, ...); */
int close(int fd);
ssize_t read(int fd, void *buf, size_t len);

#endif
