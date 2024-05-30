Queue tests:
  $ owi c -I files/normal/include files/normal/src/queue.c files/normal/src/deque.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/queue/queue_test_enqueue.c
  All OK
  $ owi c -I files/normal/include files/normal/src/queue.c files/normal/src/deque.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/queue/queue_test_poll.c
  All OK
  $ owi c -I files/normal/include files/normal/src/queue.c files/normal/src/deque.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/queue/queue_test_iter.c
  All OK
  $ owi c -I files/normal/include files/normal/src/queue.c files/normal/src/deque.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/queue/queue_test_zipIterNext.c
  All OK
