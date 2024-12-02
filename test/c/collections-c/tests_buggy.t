Bug-triggering tests:
  $ owi c -I files/bugs/include files/bugs/src/array.c files/bugs/src/common.c \
  > files/bugs/src/utils.c files/bugs/testsuite/array_test_remove.c
  Trap: memory heap buffer overflow
  model {
    symbol symbol_0 i32 8
  }
  Reached problem!
  [13]
  $ owi c -I files/bugs/include files/bugs/src/list.c files/bugs/src/common.c \
  > files/bugs/src/utils.c files/bugs/testsuite/list_test_zipIterAdd.c --no-value
  Assert failure: false
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
    symbol symbol_10 i32
    symbol symbol_2 i32
    symbol symbol_3 i32
    symbol symbol_4 i32
    symbol symbol_5 i32
    symbol symbol_6 i32
    symbol symbol_7 i32
    symbol symbol_8 i32
    symbol symbol_9 i32
  }
  Reached problem!
  [13]
