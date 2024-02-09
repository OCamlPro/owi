Queue tests:
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_enqueue.c
  All OK
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_poll.c
  All OK
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_iter.c
  All OK
  $ owi c -I include src/queue.c src/deque.c src/common.c src/utils.c testsuite/queue/queue_test_zipIterNext.c
  All OK
