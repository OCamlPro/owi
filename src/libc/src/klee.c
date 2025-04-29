#include <assert.h>
#include <klee.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define __OWI_INTERNALS
#include <owi.h>
#undef __OWI_INTERNALS

unsigned char *make_in(size_t size) {
  unsigned char *t = malloc(size);
  switch (size) {
  case 8:
    *t = owi_i8();
    break;
  case 32:
    *t = owi_i32();
    break;
  case 64:
    *t = owi_i64();
    break;
  default:
    for (int i = 0; i < (size / sizeof(char)); i++) {
      t[i] = owi_i8();
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
  owi_end_scope();
}

int klee_range(int begin, int end, const char *name) {
  owi_open_scope(name);
  int range = owi_range(begin, end);
  owi_end_scope();
  return range;
}
int klee_int(const char *name) {
  owi_open_scope(name);
  int i = owi_i32();
  owi_end_scope();
  return i;
}

void klee_silent_exit(int status) { owi_exit(status); }
void klee_abort() { owi_abort(); }
void klee_report_error(const char *file, int line, const char *message,
                       const char *suffix) {
  assert(0);
}
size_t klee_get_obj_size(void *ptr) {
  assert(0);
  return 0;
}
void klee_print_expr(const char *msg, ...) { assert(0); }

uintptr_t klee_choose(uintptr_t n) {
  assert(0);
  return 0;
}

void klee_assert(int condition) { owi_assert(condition); }
unsigned klee_is_symbolic(uintptr_t n) { assert(0); }
unsigned klee_is_replay() { assert(0); }

void klee_assume(uintptr_t condition) { owi_assume(condition); }
void klee_warning(const char *message) {}
void klee_warning_once(const char *message) {}

void klee_prefer_cex(void *object, uintptr_t condition) { assert(0); }
void klee_posix_prefer_cex(void *object, uintptr_t condition) { assert(0); }
void klee_mark_global(void *object) { assert(0); }

void klee_check_memory_access(const void *address, size_t size) { assert(0); }
void klee_set_forking(unsigned enable) { assert(0); }
void klee_stack_trace() { assert(0); }
void klee_print_range(const char *name, int arg) { assert(0); }
void klee_open_merge() { assert(0); }
void klee_close_merge() { assert(0); }
int klee_get_errno() {
  assert(0);
  return 1;
}
