// SPDX-License-Identifier: LGPL-2.1-or-later
// Copyright (C) 2012-2024 CEA (Commissariat à l'énergie atomique et aux énergies alternatives
// This file is originally from the Frama-C's E-ACSL plug-in available at https://git.frama-c.com/pub/frama-c/-/tree/master/src/plugins/e-acsl

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright © 2021-2024 OCamlPro
// Modified by the Owi programmers

#ifndef _EACSL_H_
#define _EACSL_H_

#include <stddef.h>
#include <inttypes.h>

/* Memory model */

/* Memory state initialization */
void __e_acsl_memory_init(int *argc_ref, char ***argv, size_t ptr_size);
void __e_acsl_memory_clean(void);

/* Tracking */
void *__e_acsl_store_block(void *ptr, size_t size);
void *__e_acsl_store_block_duplicate(void *ptr, size_t size);
void __e_acsl_delete_block(void *ptr);

/* Predicates */
size_t __e_acsl_offset(void *ptr);
void *__e_acsl_base_addr(void *ptr);
size_t __e_acsl_block_length(void *ptr);
int __e_acsl_valid_read(void *ptr, size_t size, void *base, void *addrof_base);
int __e_acsl_valid(void *ptr, size_t size, void *base, void *addrof_base);
int __e_acsl_initialized(void *ptr, size_t size);
int __e_acsl_freeable(void *ptr);
int __e_acsl_separated(size_t count, ...);

/* Block initialization  */
void __e_acsl_mark_readonly(void *ptr);
void __e_acsl_initialize(void *ptr, size_t size);
void __e_acsl_full_init(void *ptr);

/* Function and statement contracts */

typedef struct __e_acsl_contract_t {
  /*! \brief Number of cells in the char array used to store the results of
     * the assumes clauses.
     */
  size_t char_count;

  /*! \brief Char array to store the results of the assumes clauses. One bit
     * per behavior.
     *
     * The functions \ref find_char_index() and \ref find_bit_index() can be
     * used to find the location of the bit for a specific behavior. */
  char *assumes;
} __e_acsl_contract_t;

/* Contract initialization */
__e_acsl_contract_t *__e_acsl_contract_init(size_t size);
void __e_acsl_contract_clean(__e_acsl_contract_t *c);

/* Contract behaviors */
void __e_acsl_contract_set_behavior_assumes(__e_acsl_contract_t *c, size_t i, int assumes);
int __e_acsl_contract_get_behavior_assumes(const __e_acsl_contract_t *c, size_t i);
int __e_acsl_contract_partial_count_behaviors(const __e_acsl_contract_t *c, size_t count, ...);
int __e_acsl_contract_partial_count_all_behaviors(const __e_acsl_contract_t *c);

typedef enum __e_acsl_assert_data_type_t {
  /*! Integer data. */
  E_ACSL_INT = 0,

  /*! Real data. */
  E_ACSL_REAL,

  /*! Pointer data. */
  E_ACSL_PTR,
  /*! Array data. */
  E_ACSL_ARRAY,
  /*! Function data. */
  E_ACSL_FUN,

  /*! Structure data. */
  E_ACSL_STRUCT,
  /*! Union data. */
  E_ACSL_UNION,

  /*! Other type of data. */
  E_ACSL_OTHER = 1000
} __e_acsl_assert_data_type_t;

typedef enum __e_acsl_assert_data_ikind_t {
  E_ACSL_IBOOL,
  E_ACSL_ICHAR,
  E_ACSL_ISCHAR,
  E_ACSL_IUCHAR,
  E_ACSL_IINT,
  E_ACSL_IUINT,
  E_ACSL_ISHORT,
  E_ACSL_IUSHORT,
  E_ACSL_ILONG,
  E_ACSL_IULONG,
  E_ACSL_ILONGLONG,
  E_ACSL_IULONGLONG,
  // E_ACSL_IMPZ,
} __e_acsl_assert_data_ikind_t;

typedef union __e_acsl_assert_data_int_value_t {
  _Bool value_bool;
  char value_char;
  signed char value_schar;
  unsigned char value_uchar;
  int value_int;
  unsigned int value_uint;
  short value_short;
  unsigned short value_ushort;
  long value_long;
  unsigned long value_ulong;
  long long value_llong;
  unsigned long long value_ullong;
} __e_acsl_assert_data_int_value_t;

typedef struct __e_acsl_assert_data_int_content_t {
  /*! True if the integer is an enum value, false otherwise */
  int is_enum;
  /*! Kind of integer. */
  __e_acsl_assert_data_ikind_t kind;
  /*! Value of the integer.
      The active member of the union is identified with the field `kind`. */
  __e_acsl_assert_data_int_value_t value;
} __e_acsl_assert_data_int_content_t;

typedef enum __e_acsl_assert_data_rkind_t {
  /*! Floating point data of type float. */
  E_ACSL_RFLOAT,
  /*! Floating point data of type double. */
  E_ACSL_RDOUBLE,
  /*! Floating point data of type long double. */
  E_ACSL_RLONGDOUBLE,
} __e_acsl_assert_data_rkind_t;

typedef union __e_acsl_assert_data_real_value_t {
  float value_float;
  double value_double;
  long double value_ldouble;
} __e_acsl_assert_data_real_value_t;

typedef struct __e_acsl_assert_data_real_content_t {
  /*! Kind of real. */
  __e_acsl_assert_data_rkind_t kind;
  /*! Value of the real.
      The active member of the union is identified with the field `kind`. */
  __e_acsl_assert_data_real_value_t value;
} __e_acsl_assert_data_real_content_t;

typedef union __e_acsl_assert_data_content_t {
  __e_acsl_assert_data_int_content_t int_content;

  __e_acsl_assert_data_real_content_t real_content;

  uintptr_t value_ptr;
  uintptr_t value_array;
} __e_acsl_assert_data_content_t;

typedef struct __e_acsl_assert_data_value_t {
  /*! Name of the piece of data */
  const char *name;
  /*! Type of the piece of data */
  __e_acsl_assert_data_type_t type;
  /*! Value of the piece of data.
      The active member of the union is identified with the field `type`. */
  __e_acsl_assert_data_content_t content;
  /*! Pointer to the next value in the list, or NULL if this is the last
      value.
      We can use a list here because the number of data in an assertion is
      small enough. */
  struct __e_acsl_assert_data_value_t *next;
} __e_acsl_assert_data_value_t;

typedef struct __e_acsl_assert_data_t {
  /*! integer representing if the assertion is blocking or not */
  int blocking;
  /*! C string representing a kind of annotation (e.g., "Assertion") */
  const char *kind;
  /*! name identifying the predicate */
  const char *name;
  /*! stringified predicate */
  const char *pred_txt;
  /*! un-instrumented file of predicate placement */
  const char *file;
  /*! function of predicate placement in the un-instrumented file */
  const char *fct;
  /*! line of predicate placement in the un-instrumented file */
  int line;
  /*! values contributing to the predicate */
  __e_acsl_assert_data_value_t *values;
} __e_acsl_assert_data_t;

/* Assert */
void __e_acsl_assert(int predicate, __e_acsl_assert_data_t *data);

/* Assert data registration */
void __e_acsl_assert_register_bool(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, _Bool value);
void __e_acsl_assert_register_char(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, char value);
void __e_acsl_assert_register_schar(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, signed char value);
void __e_acsl_assert_register_uchar(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, unsigned char value);
void __e_acsl_assert_register_int(__e_acsl_assert_data_t *data, const char *name,
                               int is_enum, int value);
void __e_acsl_assert_register_uint(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, unsigned int value);
void __e_acsl_assert_register_short(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, short value);
void __e_acsl_assert_register_ushort(__e_acsl_assert_data_t *data, const char *name,
                                  int is_enum, unsigned short value);
void __e_acsl_assert_register_long(__e_acsl_assert_data_t *data, const char *name,
                                int is_enum, long value);
void __e_acsl_assert_register_ulong(__e_acsl_assert_data_t *data, const char *name,
                                 int is_enum, unsigned long value);
void __e_acsl_assert_register_longlong(__e_acsl_assert_data_t *data, const char *name,
                                    int is_enum, long long value);
void __e_acsl_assert_register_ulonglong(__e_acsl_assert_data_t *data,
                                     const char *name, int is_enum,
                                     unsigned long long value);
void __e_acsl_assert_register_float(__e_acsl_assert_data_t *data, const char *name,
                                 float value);
void __e_acsl_assert_register_double(__e_acsl_assert_data_t *data, const char *name,
                                  double value);
void __e_acsl_assert_register_longdouble(__e_acsl_assert_data_t *data,
                                      const char *name, long double value);
void __e_acsl_assert_register_ptr(__e_acsl_assert_data_t *data, const char *name,
                               void *ptr);
void __e_acsl_assert_register_array(__e_acsl_assert_data_t *data, const char *name,
                                 void *array);
void __e_acsl_assert_register_fun(__e_acsl_assert_data_t *data, const char *name);
void __e_acsl_assert_register_struct(__e_acsl_assert_data_t *data, const char *name);
void __e_acsl_assert_register_union(__e_acsl_assert_data_t *data, const char *name);
void __e_acsl_assert_register_other(__e_acsl_assert_data_t *data, const char *name);

/* Miscellaneous functions */
void __e_acsl_assert_copy_values(__e_acsl_assert_data_t *dest,
                              __e_acsl_assert_data_t *src);
void __e_acsl_assert_clean(__e_acsl_assert_data_t *data);


#endif
