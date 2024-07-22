  $ owi c --e-acsl ./else.c --no-value
  Assert failure: (i32.to_bool (i32.shr_u (i32.xor symbol_0 (i32 -1)) (i32 31)))
  Model:
    (model
      (symbol_0 i32))
  Reached problem!
  [13]
