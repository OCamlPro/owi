Pqueue tests:
  $ owi c -I include src/pqueue.c src/common.c src/utils.c testsuite/pqueue/pqueue_test_enqueue.c --no-value
  type mismatch (typecheck_expr 1)
  [35]
  $ owi c -I include src/pqueue.c src/common.c src/utils.c testsuite/pqueue/pqueue_test_pop.c --no-value
  type mismatch (typecheck_expr 1)
  [35]
