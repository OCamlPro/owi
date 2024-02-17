mul binop:
  $ dune exec owi -- sym mul_i32.wat --no-value
  Assert failure: (i32.ge (i32.mul symbol_0 symbol_1) (i32 0))
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached problem!
  [13]
  $ dune exec owi -- sym mul_i64.wat --no-value
  Assert failure: (i64.ge (i64.mul symbol_0 symbol_1) (i64 0))
  Model:
    (model
      (symbol_0 i64)
      (symbol_1 i64))
  Reached problem!
  [13]
  $ dune exec owi -- sym mul_f32.wat --no-value
  Assert failure: (f32.eq (f32.mul symbol_0 symbol_1) (f32.mul symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ dune exec owi -- sym mul_f64.wat --no-value
  Assert failure: (f64.eq (f64.mul symbol_0 symbol_1) (f64.mul symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
