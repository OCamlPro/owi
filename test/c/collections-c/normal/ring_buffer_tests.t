Ring-buffer tests:
  $ owi c -I include src/ring_buffer.c src/common.c src/utils.c testsuite/ring_buffer/ring_buffer_test_enqueue.c
  malformed section id
  [26]
  $ owi c -I include src/ring_buffer.c src/common.c src/utils.c testsuite/ring_buffer/ring_buffer_test_dequeue.c
  malformed section id
  [26]
  $ owi c -I include src/ring_buffer.c src/common.c src/utils.c testsuite/ring_buffer/ring_buffer_test_capacity.c
  malformed section id
  [26]
