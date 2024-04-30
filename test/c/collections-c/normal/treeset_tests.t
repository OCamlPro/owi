Treeset tests:
  $ owi c -I include src/treeset.c src/treetable.c src/common.c src/utils.c testsuite/treeset/treeset_test_remove.c
  type mismatch (pop)
  [35]
  $ owi c -I include src/treeset.c src/treetable.c src/common.c src/utils.c testsuite/treeset/treeset_test_iterNext.c
  type mismatch (pop)
  [35]
  $ owi c -I include src/treeset.c src/treetable.c src/common.c src/utils.c testsuite/treeset/treeset_test_removeAll.c
  type mismatch (pop)
  [35]
  $ owi c -I include src/treeset.c src/treetable.c src/common.c src/utils.c testsuite/treeset/treeset_test_iterRemove.c
  type mismatch (typecheck_expr 1)
  [35]
  $ owi c -I include src/treeset.c src/treetable.c src/common.c src/utils.c testsuite/treeset/treeset_test_add.c
  type mismatch (pop)
  [35]
  $ owi c -I include src/treeset.c src/treetable.c src/common.c src/utils.c testsuite/treeset/treeset_test_size.c
  type mismatch (pop)
  [35]
