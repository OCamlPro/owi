add binop:
  $ dune exec owi -- sym add_i32.wat
  Assert failure: (i32.le (i32 0) (i32.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (i32 1073741825))
      (symbol_1 (i32 1073741824)))
  Reached problem!
  [1]
  $ dune exec owi -- sym add_i64.wat
  Assert failure: (i64.le (i64 0) (i64.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (i64 4611686018427387905))
      (symbol_1 (i64 4611686018427387904)))
  Reached problem!
  [1]
  $ dune exec owi -- sym add_f32.wat
  Assert failure: (f32.eq (f32.add symbol_0 symbol_1) (f32.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f32 10.)))
  Reached problem!
  [1]
  $ dune exec owi -- sym add_f64.wat
  Assert failure: (f64.eq (f64.add symbol_0 symbol_1) (f64.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (f64 neg_infinity))
      (symbol_1 (f64 infinity)))
  Reached problem!
  [1]
