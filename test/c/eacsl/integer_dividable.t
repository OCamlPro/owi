  $ owi c --e-acsl integer_dividable.c --no-value
  Assert failure: (bool.eq (i32.of_bool (bool.eq (i32.rem symbol_0 symbol_1) (i32 0))) (i32 1))
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached problem!
  [13]
