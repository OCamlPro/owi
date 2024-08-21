Pqueue tests:
  $ ./run-subdir.sh pqueue -O0 --no-value
  Testing "files/normal/testsuite/pqueue/pqueue_test_enqueue.c":
  Using owi sym:
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
  Using owi conc:
  All OK
  Testing "files/normal/testsuite/pqueue/pqueue_test_pop.c":
  Using owi sym:
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
  Using owi conc:
  All OK