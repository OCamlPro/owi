// SPDX-License-Identifier: LGPL-2.1-or-later
// Copyright (C) 2012-2024 CEA (Commissariat à l'énergie atomique et aux énergies alternatives
// This file is originally from the Frama-C's E-ACSL plug-in available at https://git.frama-c.com/pub/frama-c/-/tree/master/src/plugins/e-acsl

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2021-2024 OCamlPro
// Modified by the Owi programmers

#include <eacsl.h>
#include <stddef.h>
#include <stdarg.h>
#include <limits.h>
#include <owi.h>

extern void *malloc(size_t size);
extern void *calloc(size_t, size_t);
__attribute__((import_module("owi"), import_name("dealloc"))) void *free(void *);

/* Memory model */

/* Memory state initialization */
void __e_acsl_memory_init(int *argc_ref, char ***argv, size_t ptr_size) {}
void __e_acsl_memory_clean(void) {}

/* Tracking */
void *__e_acsl_store_block(void *ptr, size_t size) {}
void *__e_acsl_store_block_duplicate(void *ptr, size_t size) {}
void __e_acsl_delete_block(void *ptr) {}

/* Predicates */
size_t __e_acsl_offset(void *ptr) {}
void *__e_acsl_base_addr(void *ptr) {}
size_t __e_acsl_block_length(void *ptr) {}
int __e_acsl_valid_read(void *ptr, size_t size, void *base, void *addrof_base) { return 1; }
int __e_acsl_valid(void *ptr, size_t size, void *base, void *addrof_base) { return 1; }
int __e_acsl_initialized(void *ptr, size_t size) { return 1; }
int __e_acsl_freeable(void *ptr) { return 1; }
int __e_acsl_separated(size_t count, ...) { return 1; }

/* Block initialization  */
void __e_acsl_mark_readonly(void *ptr) {}
void __e_acsl_initialize(void *ptr, size_t size) {}
void __e_acsl_full_init(void *ptr) {}

/* Function and statement contracts */

/* Contract initialization */
static inline size_t find_char_index(size_t global_bit_index) {
  return global_bit_index / CHAR_BIT;
}

static inline size_t find_bit_index(size_t global_bit_index) {
  return global_bit_index % CHAR_BIT;
}

static inline size_t find_char_count(size_t bit_count) {
  size_t char_count = bit_count / CHAR_BIT;
  if (bit_count % CHAR_BIT > 0) {
    ++char_count;
  }
  return char_count;
}

static inline int normalize_to_boolean(int value) {
  return value ? 1 : 0;
}

__e_acsl_contract_t *__e_acsl_contract_init(size_t size) {
  // Allocate memory for the structure
  __e_acsl_contract_t *c = malloc(sizeof(__e_acsl_contract_t));
  // DVASSERT(c != NULL, "Unable to allocate %d bytes of memory for contract_t\n",
  //          sizeof(__e_acsl_contract_t));

  // Compute the number of char needed to store `size` behaviors, assuming
  // that one behavior is stored in one bit.
  c->char_count = find_char_count(size);

  // Allocate an array of char of the computed count
  if (c->char_count > 0) {
    c->assumes = calloc(c->char_count, sizeof(char));
    // DVASSERT(c->assumes != NULL,
    //          "Unable to allocate %d cells of %d bytes of memory for "
    //          "contract_t::assumes\n",
    //          c->char_count, sizeof(char));
  } else {
    c->assumes = NULL;
  }

  return c;
}

void __e_acsl_contract_clean(__e_acsl_contract_t *c) {
  // Free array of char
  free(c->assumes);
  // Free structure
  free(c);
}

/* Contract behaviors */
void __e_acsl_contract_set_behavior_assumes(__e_acsl_contract_t *c, size_t i, int assumes) {
  size_t char_idx = find_char_index(i);
  // DVASSERT(char_idx < c->char_count,
  //          "Out of bound char index %d (char_count: %d)\n", char_idx,
  //          c->char_count);
  size_t bit_idx = find_bit_index(i);
  assumes = normalize_to_boolean(assumes);
  c->assumes[char_idx] |= (assumes << bit_idx);
}

int __e_acsl_contract_get_behavior_assumes(const __e_acsl_contract_t *c, size_t i) {
  size_t char_idx = find_char_index(i);
  // DVASSERT(char_idx < c->char_count,
  //          "Out of bound char index %d (char_count: %d)\n", char_idx,
  //          c->char_count);
  size_t bit_idx = find_bit_index(i);
  int result = c->assumes[char_idx] & (1 << bit_idx);
  return normalize_to_boolean(result);
}

int __e_acsl_contract_partial_count_behaviors(const __e_acsl_contract_t *c, size_t count, ...) {
  va_list args;
  va_start(args, count);

  int result = 0;
  for (size_t i = 0; i < 2 && i < count; ++i) {
    result += __e_acsl_contract_get_behavior_assumes(c, va_arg(args, int));
  }

  va_end(args);
  return result;
}

int __e_acsl_contract_partial_count_all_behaviors(const __e_acsl_contract_t *c) {
  int result = 0;
  for (int i = 0; i < c->char_count && result < 2; ++i) {
    // Counting bits set with Kernighan's algorithm, but stopping at two
    // bits set.
    // cf. <https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan>
    unsigned char assumes_cell = c->assumes[i];
    for (; assumes_cell && result < 2; ++result) {
      assumes_cell &= assumes_cell - 1;
    }
  }
  return result;
}

/* Assertions */

/* Assert */
void __e_acsl_assert(int predicate, __e_acsl_assert_data_t *data) {
    owi_assert(predicate);
}

/* Assert data registration */
void __e_acsl_assert_register_bool(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, _Bool value) {}
void __e_acsl_assert_register_char(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, char value) {}
void __e_acsl_assert_register_schar(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, signed char value) {}
void __e_acsl_assert_register_uchar(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, unsigned char value) {}
void __e_acsl_assert_register_int(__e_acsl_assert_data_t *data, const char *name,
                               int is_enum, int value) {}
void __e_acsl_assert_register_uint(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, unsigned int value) {}
void __e_acsl_assert_register_short(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, short value) {}
void __e_acsl_assert_register_ushort(__e_acsl_assert_data_t *data, const char *name,
                                  int is_enum, unsigned short value) {}
void __e_acsl_assert_register_long(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, long value) {}
void __e_acsl_assert_register_ulong(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, unsigned long value) {}
void __e_acsl_assert_register_longlong(__e_acsl_assert_data_t *data, const char *name,
                                    int is_enum, long long value) {}
void __e_acsl_assert_register_ulonglong(__e_acsl_assert_data_t *data,
                                     const char *name, int is_enum,
                                     unsigned long long value) {}
void __e_acsl_assert_register_generic_real(__e_acsl_assert_data_t *data,
                                        const char *name,
                                        __e_acsl_assert_data_rkind_t kind,
                                        __e_acsl_assert_data_real_value_t value) {}
void __e_acsl_assert_register_float(__e_acsl_assert_data_t *data, const char *name,
                                 float value) {}
void __e_acsl_assert_register_double(__e_acsl_assert_data_t *data, const char *name,
                                  double value) {}
void __e_acsl_assert_register_longdouble(__e_acsl_assert_data_t *data,
                                      const char *name, long double value) {}
void __e_acsl_assert_register_ptr(__e_acsl_assert_data_t *data, const char *name,
                               void *ptr) {}
void __e_acsl_assert_register_array(__e_acsl_assert_data_t *data, const char *name,
                                 void *array) {}
void __e_acsl_assert_register_fun(__e_acsl_assert_data_t *data, const char *name) {}
void __e_acsl_assert_register_struct(__e_acsl_assert_data_t *data, const char *name) {}
void __e_acsl_assert_register_union(__e_acsl_assert_data_t *data, const char *name) {}
void __e_acsl_assert_register_other(__e_acsl_assert_data_t *data, const char *name) {}
void __e_acsl_assert_free_value(__e_acsl_assert_data_value_t *value) {}
void __e_acsl_assert_copy_value(__e_acsl_assert_data_t *dest,
                             __e_acsl_assert_data_value_t *value) {}
void __e_acsl_assert_copy_values(__e_acsl_assert_data_t *dest,
                              __e_acsl_assert_data_t *src) {}
void __e_acsl_assert_clean(__e_acsl_assert_data_t *data) {}
