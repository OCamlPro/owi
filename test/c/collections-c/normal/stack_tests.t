Stack tests:
  $ owi c -I include src/stack.c src/array.c src/common.c src/utils.c testsuite/stack/stack_test_pop.c
  type mismatch (typecheck_expr 1)
  [35]
  $ owi c -I include src/stack.c src/array.c src/common.c src/utils.c testsuite/stack/stack_test_push.c
  type mismatch (typecheck_expr 1)
  [35]
