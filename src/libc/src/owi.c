#include <owi.h>

int and_(int a, int b) {
  __asm__ __volatile__("local.get 0;"
                       "i32.const 0;"
                       "i32.ne;"
                       "local.get 1;"
                       "i32.const 0;"
                       "i32.ne;"
                       "i32.and;"
                       "return;");
}

int or_(int a, int b) {
  __asm__ __volatile__("local.get 0;"
                       "i32.const 0;"
                       "i32.ne;"
                       "local.get 1;"
                       "i32.const 0;"
                       "i32.ne;"
                       "i32.or;"
                       "return;");
}
