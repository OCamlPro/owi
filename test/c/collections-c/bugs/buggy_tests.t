Bug-triggering tests:
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array_test_remove.c
  Trap: memory heap buffer overflow
  Model:
    (model
      (symbol_0 (i32 8)))
  Reached problem!
  found a bug while performing symbolic execution!
  [13]
  $ owi c -I include src/list.c src/common.c src/utils.c testsuite/list_test_zipIterAdd.c --no-value
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Assert failure: false
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32)
      (symbol_10 i32)
      (symbol_2 i32)
      (symbol_3 i32)
      (symbol_4 i32)
      (symbol_5 i32)
      (symbol_6 i32)
      (symbol_7 i32)
      (symbol_8 i32)
      (symbol_9 i32))
  Reached problem!
  found a bug while performing symbolic execution!
  [13]
