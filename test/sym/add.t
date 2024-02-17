add binop:
  $ dune exec owi -- sym add_i32.wat --no-value
  Assert failure: (i32.le (i32 0) (i32.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  found a bug while performing symbolic execution!
  [13]
  $ dune exec owi -- sym add_i64.wat --no-value
  Assert failure: (i64.le (i64 0) (i64.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 i64)
      (symbol_1 i64))
  found a bug while performing symbolic execution!
  [13]
  $ dune exec owi -- sym add_f32.wat --no-value
  Assert failure: (f32.eq (f32.add symbol_0 symbol_1) (f32.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  found a bug while performing symbolic execution!
  [13]
  $ dune exec owi -- sym add_f64.wat --no-value
  Assert failure: (f64.eq (f64.add symbol_0 symbol_1) (f64.add symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  found a bug while performing symbolic execution!
  [13]
