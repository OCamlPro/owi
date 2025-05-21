#include <pthread.h>

int pthread_mutexattr_init(pthread_mutexattr_t *attr) { return 0; }
int pthread_mutexattr_destroy(pthread_mutexattr_t *attr) { return 0; }

int pthread_mutexattr_getkind_np(const pthread_mutexattr_t *attr, int *kind) {
  return 0;
}
int pthread_mutexattr_setkind_np(pthread_mutexattr_t *attr, int kind) {
  return 0;
}

int pthread_mutex_init(pthread_mutex_t *mutex,
                       const pthread_mutexattr_t *mutexattr) {
  return 0;
}
int pthread_mutex_lock(pthread_mutex_t *mutex) { return 0; }
int pthread_mutex_unlock(pthread_mutex_t *mutex) { return 0; }
int pthread_mutex_trylock(pthread_mutex_t *mutex) { return 0; }
int pthread_mutex_destroy(pthread_mutex_t *mutex) { return 0; }

int pthread_cond_init(pthread_cond_t *cond, pthread_condattr_t *cond_attr) {
  return 0;
}
int pthread_cond_destroy(pthread_cond_t *cond) { return 0; }
int pthread_cond_signal(pthread_cond_t *cond) { return 0; }
int pthread_cond_broadcast(pthread_cond_t *cond) { return 0; }
int pthread_cond_timedwait(pthread_cond_t *cond, pthread_mutex_t *mutex,
                           const struct timespec *abstime) {
  return 0;
}
int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex) {
  return 0;
}

int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type) { return 0; }

int pthread_rwlock_init(pthread_rwlock_t *rwlock,
                        const pthread_rwlockattr_t *attr) {
  return 0;
}
int pthread_rwlock_destroy(pthread_rwlock_t *rwlock) { return 0; }

int pthread_rwlock_rdlock(pthread_rwlock_t *rwlock) { return 0; }
int pthread_rwlock_tryrdlock(pthread_rwlock_t *rwlock) { return 0; }
int pthread_rwlock_wrlock(pthread_rwlock_t *rwlock) { return 0; }
int pthread_rwlock_trywrlock(pthread_rwlock_t *rwlock) { return 0; }
int pthread_rwlockattr_init(pthread_rwlockattr_t *attr) { return 0; }
int pthread_rwlockattr_destroy(pthread_rwlockattr_t *attr) { return 0; }
int pthread_rwlock_unlock(pthread_rwlock_t *rwlock) { return 0; }

int pthread_detach(pthread_t __th) { return 0; }
int pthread_once(pthread_once_t *once_control, void (*init_routine)(void)) {
  return 0;
}
int pthread_attr_init(pthread_attr_t *attr) { return 0; }
int pthread_attr_destroy(pthread_attr_t *attr) { return 0; }
int pthread_attr_setstacksize(pthread_attr_t *attr, const size_t stacksize) {
  return 0;
}
int pthread_create(pthread_t *__threadarg, const pthread_attr_t *__attr,
                   void *(*__start_routine)(void *), void *__arg) {
  return 0;
}
int pthread_join(pthread_t __th, void **__thread_return) { return 0; }
pthread_t pthread_self(void) { return 0; }
int pthread_equal(pthread_t __thread1, pthread_t __thread2) { return 0; }
