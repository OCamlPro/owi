#ifndef PTHREAD_H
#define PTHREAD_H

#include <sched.h>

#define PTHREAD_STACK_MIN 16384
#define PTHREAD_STACK_MAX (8 << 20)

struct _pthread_descr_struct {
  int empty;
};

typedef struct _pthread_descr_struct *_pthread_descr;
typedef int pthread_t;

/* Fast locks */
struct _pthread_fastlock {
  int __spinlock;
};

#define PTHREAD_SPIN_LOCKED 1
#define PTHREAD_SPIN_UNLOCKED 0

/* Mutexes */
typedef struct {
  struct _pthread_fastlock lock;
  _pthread_descr owner;
  int kind;
  unsigned int count;
} pthread_mutex_t;

enum {
  PTHREAD_MUTEX_FAST_NP,
#define PTHREAD_MUTEX_FAST_NP PTHREAD_MUTEX_FAST_NP
#define PTHREAD_MUTEX_NORMAL PTHREAD_MUTEX_FAST_NP
  PTHREAD_MUTEX_RECURSIVE_NP,
#define PTHREAD_MUTEX_RECURSIVE_NP PTHREAD_MUTEX_RECURSIVE_NP
  PTHREAD_MUTEX_ERRORCHECK_NP,
#define PTHREAD_MUTEX_ERRORCHECK_NP PTHREAD_MUTEX_ERRORCHECK_NP
};

enum {
  PTHREAD_PROCESS_PRIVATE,
#define PTHREAD_PROCESS_PRIVATE PTHREAD_PROCESS_PRIVATE
  PTHREAD_PROCESS_SHARED
#define PTHREAD_PROCESS_SHARED PTHREAD_PROCESS_SHARED
};

#define PTHREAD_MUTEX_INITIALIZER                                              \
  { {PTHREAD_SPIN_UNLOCKED}, 0, PTHREAD_MUTEX_FAST_NP, 0 }

#define PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP                                 \
  { {PTHREAD_SPIN_UNLOCKED}, 0, PTHREAD_MUTEX_RECURSIVE_NP, 0 }

#define PTHREAD_ERRORCHECK_MUTEX_INITIALIZER_NP                                \
  { {PTHREAD_SPIN_UNLOCKED}, 0, PTHREAD_MUTEX_ERRORCHECK_NP, 0 }

typedef struct {
  int __mutexkind;
} pthread_mutexattr_t;

int pthread_mutexattr_init(pthread_mutexattr_t *attr);
int pthread_mutexattr_destroy(pthread_mutexattr_t *attr);

int pthread_mutexattr_getkind_np(const pthread_mutexattr_t *attr, int *kind);
int pthread_mutexattr_setkind_np(pthread_mutexattr_t *attr, int kind);

int pthread_mutex_init(pthread_mutex_t *mutex,
                       const pthread_mutexattr_t *mutexattr);
int pthread_mutex_lock(pthread_mutex_t *mutex);
int pthread_mutex_unlock(pthread_mutex_t *mutex);
int pthread_mutex_trylock(pthread_mutex_t *mutex);
int pthread_mutex_destroy(pthread_mutex_t *mutex);

int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type);

typedef void *pthread_condattr_t;

typedef struct {
  struct _pthread_fastlock lock;
  _pthread_descr wait_chain;
} pthread_cond_t;

#define PTHREAD_COND_INITIALIZER                                               \
  { {PTHREAD_SPIN_UNLOCKED}, 0 }

int pthread_cond_init(pthread_cond_t *cond, pthread_condattr_t *cond_attr);
int pthread_cond_destroy(pthread_cond_t *cond);
int pthread_cond_signal(pthread_cond_t *cond);
int pthread_cond_broadcast(pthread_cond_t *cond);
int pthread_cond_timedwait(pthread_cond_t *cond, pthread_mutex_t *mutex,
                           const struct timespec *abstime);
int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex);

typedef int pthread_once_t;

/* Attributes for threads.  */
typedef struct {
  int __detachstate;
  int __schedpolicy;
  struct sched_param __schedparam;
  int __inheritsched;
  int __scope;
  void *__stackaddr;
  unsigned long __stacksize;
} pthread_attr_t;

enum {
  PTHREAD_CREATE_JOINABLE,
#define PTHREAD_CREATE_JOINABLE PTHREAD_CREATE_JOINABLE
  PTHREAD_CREATE_DETACHED
#define PTHREAD_CREATE_DETACHED PTHREAD_CREATE_DETACHED
};

enum {
  PTHREAD_EXPLICIT_SCHED,
#define PTHREAD_EXPLICIT_SCHED PTHREAD_EXPLICIT_SCHED
  PTHREAD_INHERIT_SCHED
#define PTHREAD_INHERIT_SCHED PTHREAD_INHERIT_SCHED
};

enum { /* for completeness */
       PTHREAD_SCOPE_SYSTEM,
#define PTHREAD_SCOPE_SYSTEM PTHREAD_SCOPE_SYSTEM
       PTHREAD_SCOPE_PROCESS
#define PTHREAD_SCOPE_PROCESS PTHREAD_SCOPE_PROCESS
};

typedef struct {
  unsigned int n;
} pthread_rwlock_t;

typedef struct {
  int dummy;
} pthread_rwlockattr_t;

#define PTHREAD_RWLOCK_INITIALIZER                                             \
  { 0 }

int pthread_rwlock_init(pthread_rwlock_t *rwlock,
                        const pthread_rwlockattr_t *attr);
int pthread_rwlock_destroy(pthread_rwlock_t *rwlock);
// pthread_rwlock_t rwlock=PTHREAD_RWLOCK_INITIALIZER;
int pthread_rwlock_rdlock(pthread_rwlock_t *rwlock);
int pthread_rwlock_tryrdlock(pthread_rwlock_t *rwlock);
int pthread_rwlock_wrlock(pthread_rwlock_t *rwlock);
int pthread_rwlock_trywrlock(pthread_rwlock_t *rwlock);
int pthread_rwlockattr_init(pthread_rwlockattr_t *attr);
int pthread_rwlockattr_destroy(pthread_rwlockattr_t *attr);
int pthread_rwlock_unlock(pthread_rwlock_t *rwlock);

int pthread_detach(pthread_t __th);
int pthread_once(pthread_once_t *once_control, void (*init_routine)(void));
int pthread_attr_init(pthread_attr_t *attr);
int pthread_attr_destroy(pthread_attr_t *attr);
int pthread_attr_setstacksize(pthread_attr_t *attr, const size_t stacksize);
int pthread_create(pthread_t *__threadarg, const pthread_attr_t *__attr,
                   void *(*__start_routine)(void *), void *__arg);
int pthread_join(pthread_t __th, void **__thread_return);
pthread_t pthread_self(void);
int pthread_equal(pthread_t __thread1, pthread_t __thread2);

#endif
