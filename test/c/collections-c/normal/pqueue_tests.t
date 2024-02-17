Pqueue tests:
  $ owi c -I include src/pqueue.c src/common.c src/utils.c testsuite/pqueue/pqueue_test_enqueue.c --no-value
  Trap: memory heap buffer overflow
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32))
  Reached problem!
  [13]
  $ owi c -I include src/pqueue.c src/common.c src/utils.c testsuite/pqueue/pqueue_test_pop.c --no-value
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
