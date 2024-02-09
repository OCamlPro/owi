Array tests:
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_add.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_addAt2.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_contains.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_deepCopy.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_getAt.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_indexOf.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_iterAdd.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_iterRemove.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_iterReplace.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_reduce.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_remove.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_removeAll.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_removeAt.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_replaceAt.c
  Assert failure: (i32.ne symbol_3 symbol_2)
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 0))
      (symbol_2 (i32 0))
      (symbol_3 (i32 0)))
  Reached problem!
  [1]
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_reverse.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_shallowCopy.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_subarray.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_zipIterAdd.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_zipIterNext.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_zipIterRemove.c
  All OK
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array/array_test_zipIterReplace.c
  All OK
