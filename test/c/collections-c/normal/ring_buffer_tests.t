Ring-buffer tests:
  $ owi c -I include src/ring_buffer.c src/common.c src/utils.c testsuite/ring_buffer/ring_buffer_test_enqueue.c
  All OK
  $ owi c -I include src/ring_buffer.c src/common.c src/utils.c testsuite/ring_buffer/ring_buffer_test_dequeue.c
  All OK
  $ owi c -I include src/ring_buffer.c src/common.c src/utils.c testsuite/ring_buffer/ring_buffer_test_capacity.c
  All OK
