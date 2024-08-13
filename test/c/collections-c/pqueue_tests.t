Pqueue tests:
  $ owi c -O0 -I files/normal/include files/normal/src/pqueue.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/pqueue/pqueue_test_enqueue.c --no-value
  Trap: memory heap buffer overflow
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32))
  Reached problem!Segmentation fault
  [139]
  $ owi c -O0 -I files/normal/include files/normal/src/pqueue.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/pqueue/pqueue_test_pop.c --no-value
  Trap: memory heap buffer overflow
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32))
  Reached problem!
  [13]
