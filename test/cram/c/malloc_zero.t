  $ owi c ./malloc_zero.c --entry-point=f1 -w1 -O0
  owi: [ERROR] Assert failure: false
  model
  owi: [ERROR] Reached problem!
  [13]
  $ owi c ./malloc_zero.c --entry-point=f2 -w1 -O0
  All OK!
  $ owi c ./malloc_zero.c --entry-point=g1 -w1 -O0
  owi: [ERROR] Assert failure: false
  model
  owi: [ERROR] Reached problem!
  [13]
  $ owi c ./malloc_zero.c --entry-point=g2 -w1 -O0
  All OK!
  $ owi c ./malloc_zero.c --entry-point=h -w1 -O0
  All OK!
