  $ owi c ./malloc_zero.c --entry-point=f1 -w1 -O0
  Assert failure: false
  model
  Reached problem!
  [13]
  $ owi c ./malloc_zero.c --entry-point=f2 -w1 -O0
  All OK
  $ owi c ./malloc_zero.c --entry-point=g1 -w1 -O0
  Assert failure: false
  model
  Reached problem!
  [13]
  $ owi c ./malloc_zero.c --entry-point=g2 -w1 -O0
  All OK
  $ owi c ./malloc_zero.c --entry-point=h -w1 -O0
  All OK
