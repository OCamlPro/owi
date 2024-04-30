Bug-triggering tests:
  $ owi c -I include src/array.c src/common.c src/utils.c testsuite/array_test_remove.c
  type mismatch (pop)
  [35]
  $ owi c -I include src/list.c src/common.c src/utils.c testsuite/list_test_zipIterAdd.c --no-value
  type mismatch (pop)
  [35]
