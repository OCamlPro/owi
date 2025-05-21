#ifndef FCNTL_H
#define FCNTL_H

#define F_LINUX_SPECIFIC_BASE 1024

#define O_ACCMODE 0003
#define O_RDONLY 00
#define O_WRONLY 01
#define O_RDWR 02
#define O_CREAT 0100  /* not fcntl */
#define O_EXCL 0200   /* not fcntl */
#define O_NOCTTY 0400 /* not fcntl */
#define O_TRUNC 01000 /* not fcntl */
#define O_APPEND 02000
#define O_NONBLOCK 04000
#define O_NDELAY O_NONBLOCK
#define O_DSYNC 010000
#define FASYNC 020000   /* fcntl, for BSD compatibility */
#define O_DIRECT 040000 /* direct disk access hint - currently ignored */
#define O_LARGEFILE 0100000
#define O_DIRECTORY 0200000 /* must be a directory */
#define O_NOFOLLOW 0400000  /* don't follow links */
#define O_NOATIME 01000000
#define O_CLOEXEC 02000000
#define O_SYNC (O_DSYNC | 04000000)
#define O_PATH 010000000
#define __O_TMPFILE 020000000

#define F_DUPFD 0 /* dup */
#define F_GETFD 1 /* get close_on_exec */
#define F_SETFD 2 /* set/clear close_on_exec */
#define F_GETFL 3 /* get file->f_flags */
#define F_SETFL 4 /* set file->f_flags */
#define F_GETLK 5
#define F_SETLK 6
#define F_SETLKW 7
#define F_SETOWN 8  /*  for sockets. */
#define F_GETOWN 9  /*  for sockets. */
#define F_SETSIG 10 /*  for sockets. */
#define F_GETSIG 11 /*  for sockets. */

#define F_GETLK64 12 /*  using 'struct flock64' */
#define F_SETLK64 13
#define F_SETLKW64 14

#define FD_CLOEXEC 1 /* actually anything with low bit set goes */

/* for posix fcntl() and lockf() */
#define F_RDLCK 0
#define F_WRLCK 1
#define F_UNLCK 2

/* for old implementation of bsd flock () */
#define F_EXLCK 4 /* or 3 */
#define F_SHLCK 8 /* or 4 */

/* for leases */
#define F_INPROGRESS 16

/* operations for bsd flock(), also used by the kernel implementation */
#define LOCK_SH 1 /* shared lock */
#define LOCK_EX 2 /* exclusive lock */
#define LOCK_NB                                                                \
  4               /* or'd with one of the above to prevent                     \
                     blocking */
#define LOCK_UN 8 /* remove lock */

#define LOCK_MAND 32   /* This is a mandatory flock */
#define LOCK_READ 64   /* ... Which allows concurrent read operations */
#define LOCK_WRITE 128 /* ... Which allows concurrent write operations */
#define LOCK_RW 192    /* ... Which allows concurrent read & write ops */

#endif
