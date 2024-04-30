Queue tests:
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_enqueue.c
  type mismatch (typecheck_expr 1)
  [35]
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_poll.c
  type mismatch (typecheck_expr 1)
  [35]
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_iter.c
  type mismatch (typecheck_expr 1)
  [35]
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_zipIterNext.c
  type mismatch (typecheck_expr 1)
  [35]
