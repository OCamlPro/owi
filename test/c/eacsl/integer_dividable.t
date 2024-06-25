  $ owi c --e-acsl integer_dividable.c --no-value
  [kernel] Parsing integer_dividable.c (with preprocessing)
  [e-acsl] beginning translation.
  [kernel] Parsing FRAMAC_SHARE/e-acsl/e_acsl.h (with preprocessing)
  [e-acsl] translation done in project "e-acsl".
  Assert failure: (bool.eq (i32.of_bool (bool.eq (i32.rem symbol_0 symbol_1) (i32 0))) (i32 1))
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached problem!
