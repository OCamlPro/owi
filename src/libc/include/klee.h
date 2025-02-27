#ifndef _KLEE_H
#define _KLEE_H

#include <stdint.h>
#include <stddef.h>

void klee_define_fixed_object(void *addr, size_t nbytes);
void klee_make_symbolic(void *addr, size_t nbytes, const char *name);
int klee_range(int begin, int end, const char *name);
int klee_int(const char *name);
void klee_silent_exit(int status);
void klee_abort(void);
void klee_report_error(const char *file, int line, const char *message, const char *suffix);
size_t klee_get_obj_size(void *ptr);
void klee_print_expr(const char *msg, ...);
uintptr_t klee_choose(uintptr_t n);

void klee_assert(int);
unsigned klee_is_symbolic(uintptr_t n);
unsigned klee_is_replay(void);

void klee_assume(uintptr_t condition);
void klee_report_error(const char *file, int line, const char *message, const char *suffix);
void klee_warning(const char *message);
void klee_warning_once(const char *message);
void klee_prefer_cex(void *object, uintptr_t condition);
void klee_posix_prefer_cex(void *object, uintptr_t condition);
void klee_mark_global(void *object);

void klee_check_memory_access(const void *address, size_t size);
void klee_set_forking(unsigned enable);
void klee_stack_trace(void);
void klee_print_range(const char * name, int arg);
void klee_open_merge(void);
void klee_close_merge(void);
int klee_get_errno(void);

#endif
