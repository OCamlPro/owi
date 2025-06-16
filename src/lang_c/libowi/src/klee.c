#include <klee.h>
#include <stddef.h>
#include <stdint.h>

#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

extern void *memcpy(void *dst, const void *src, size_t n);
extern void *malloc(size_t size);

unsigned char *make_in(size_t size) {
  unsigned char *t = malloc(size);
  switch (size) {
  case 8:
    *t = owi_int8();
    break;
  case 32:
    *t = owi_int32();
    break;
  case 64:
    *t = owi_int64();
    break;
  default:
    for (int i = 0; i < (size / sizeof(char)); i++) {
      t[i] = owi_int8();
    }
    break;
  }
  return t;
}

void klee_define_fixed_object(void *addr, size_t nbytes) {
  memcpy(addr, make_in(nbytes), nbytes);
}

void klee_make_symbolic(void *addr, size_t nbytes, const char *name) {
  owi_open_scope(name);
  memcpy(addr, make_in(nbytes), nbytes);
  owi_close_scope();
}

int klee_range(int begin, int end, const char *name) {
  owi_open_scope(name);
  int range = owi_range(begin, end);
  owi_close_scope();
  return range;
}
int klee_int(const char *name) {
  owi_open_scope(name);
  int i = owi_int32();
  owi_close_scope();
  return i;
}

void klee_silent_exit(int status) { exit(status); }

__attribute__((import_module("owi"), import_name("abort"))) void abort(void);
void klee_abort() { abort(); }
void klee_report_error(const char *file, int line, const char *message,
                       const char *suffix) {
  owi_assert(0);
}
size_t klee_get_obj_size(void *ptr) {
  owi_assert(0);
  return 0;
}
void klee_print_expr(const char *msg, ...) { owi_assert(0); }

uintptr_t klee_choose(uintptr_t n) {
  owi_assert(0);
  return 0;
}

void klee_assert(int condition) { owi_assert(condition); }
unsigned klee_is_symbolic(uintptr_t n) { owi_assert(0); }
unsigned klee_is_replay() { owi_assert(0); }

void klee_assume(uintptr_t condition) { owi_assume(condition); }
void klee_warning(const char *message) {}
void klee_warning_once(const char *message) {}

void klee_prefer_cex(void *object, uintptr_t condition) { owi_assert(0); }
void klee_posix_prefer_cex(void *object, uintptr_t condition) { owi_assert(0); }
void klee_mark_global(void *object) { owi_assert(0); }

void klee_check_memory_access(const void *address, size_t size) { owi_assert(0); }
void klee_set_forking(unsigned enable) { owi_assert(0); }
void klee_stack_trace() { owi_assert(0); }
void klee_print_range(const char *name, int arg) { owi_assert(0); }
void klee_open_merge() { owi_assert(0); }
void klee_close_merge() { owi_assert(0); }
int klee_get_errno() {
  owi_assert(0);
  return 1;
}
