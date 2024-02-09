/*
 * Collections-C
 * Copyright (C) 2013-2016 Srđan Panić <i@srdja.me>
 *
 * This file is part of Collections-C.
 *
 * Collections-C is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Collections-C is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Collections-C.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "common.h"

/**
 * String comparator function.
 *
 * @param[in] str1 first string
 * @param[in] str2 second string
 *
 * @return
 */
int cc_common_cmp_str(const void *str1, const void *str2) {
  return strcmp((const char *)str1, (const char *)str2);
}

/* extern unsigned char __heap_base; */
/* unsigned int bump_pointer = &__heap_base; */

/* void *malloc(unsigned int size) { */
/*   unsigned int r = bump_pointer; */
/*   for (int i = 0; i < size; ++i) */
/*     *((unsigned char *)bump_pointer + i) = 'i'; */
/*   bump_pointer += size; */
/*   return (void*)alloc(r, size); */
/* } */

/* void *calloc(unsigned int nmemb, unsigned int size) { */
/*   unsigned int r = bump_pointer; */
/*   for (int i = 0; i < nmemb * size; ++i) */
/*     *((unsigned char *)bump_pointer + i) = 0; */
/*   bump_pointer += (nmemb * size); */
/*   return (void*)alloc(r, nmemb * size); */
/* } */

/* void free(void *ptr) { */
/*   dealloc(ptr); */
/* } */

/* void *memset(void *s, int c, size_t n) { */
/*   char *ptr; */

/*   for (ptr = (char*)s; ptr < (ptr + n); ++ptr) */
/*     *ptr = c & 0xff; */
/* } */

/* int strcmp(const char *s1, const char *s2) { */
/*   int i, f; */
/*   const char *s1_ptr; */
/*   const char *s2_ptr; */

/*   f = 0; */
/*   s1_ptr = (const char*) s1, s2_ptr = (const char*) s2; */

/*   if ((s1_ptr != NULL) && (s2_ptr != NULL)) */
/*     for (i = 0; *s1_ptr != 0; ++s1_ptr, ++s2_ptr) */
/*       if (*s1_ptr != *s2_ptr) f = 1; */

/*   return f; */
/* } */

/* void *memcpy(void *dst, const void *src, unsigned int n) { */
/*   int i; */
/*   char *dst_ptr = (char*)dst; */
/*   const char *src_ptr = (const char*)src; */

/*   if ((dst_ptr != NULL) && (src_ptr != NULL)) */
/*     for (i = 0; i < n; ++i, ++dst_ptr, ++src_ptr) */
/*       *dst_ptr = *src_ptr; */

/*   return dst; */
/* } */

/* void *memmove(void *dst, const void *src, unsigned int n) { */

/*   char *dst_ptr = (char*)dst; */
/*   const char *src_ptr = (const char*)src; */

/*   char tmp[n]; */

/*   for (int i = 0; i < n; ++i) */
/*     tmp[i] = src_ptr[i]; */

/*   for (int i = 0; i < n; ++i) */
/*     dst_ptr[i] = tmp[i]; */

/*   return dst; */
/* } */

/* void qsort(void *base, size_t nmemb, size_t size, */
/*     int (*compar)(const void *, const void *)) { */
/*   int i, j, min; */
/*   char *base_ptr, *lo; */

/*   base_ptr = (char *)base; lo = base_ptr; */

/*   for (i = 0; i < nmemb - 1; ++i) { */
/*     min = i; */
/*     for (j = i + 1; j < nmemb; ++j) */
/*       if ((*compar)((void *)(lo + j * size), (void *)(lo + min * size)) < 0)
 */
/*         min = j; */

/*     // swap 8 bytes */
/*     unsigned long *a = (unsigned long *)(lo + min * size); */
/*     unsigned long *b = (unsigned long *)(lo + i * size); */
/*     unsigned long tmp = *a; */
/*     *a = *b; */
/*     *b = tmp; */
/* #if 0 */
/*     // swap each byte one by one */
/*     size_t _n = size; */
/*     char *a = lo + min * size; */
/*     char *b = lo + i * size; */
/*     do { */
/*       char tmp = *a; */
/*       *a++ = *b; */
/*       *b++ = tmp; */
/*     } while(--_n > 0); */
/* #endif */
/*   } */
/* } */
