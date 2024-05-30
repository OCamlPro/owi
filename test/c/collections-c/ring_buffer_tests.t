Ring-buffer tests:
  $ owi c -I files/normal/include files/normal/src/ring_buffer.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/ring_buffer/ring_buffer_test_enqueue.c
  All OK
  $ owi c -I files/normal/include files/normal/src/ring_buffer.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/ring_buffer/ring_buffer_test_dequeue.c
  All OK
  $ owi c -I files/normal/include files/normal/src/ring_buffer.c files/normal/src/common.c files/normal/src/utils.c files/normal/testsuite/ring_buffer/ring_buffer_test_capacity.c
  All OK
