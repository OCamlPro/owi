sub binop:
  $ owi sym sub_i32.wat --no-value --deterministic-result-order
  Assert failure: (i32.ge symbol_0 (i32.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached problem!
  [13]
  $ owi sym sub_i64.wat --no-value --deterministic-result-order
  Assert failure: (i64.ge symbol_0 (i64.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 i64)
      (symbol_1 i64))
  Reached problem!
  [13]
  $ owi sym sub_f32.wat --no-value --deterministic-result-order
  Assert failure: (f32.eq (f32.sub symbol_0 symbol_1) (f32.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym sub_f64.wat --no-value --deterministic-result-order
  Assert failure: (f64.eq (f64.sub symbol_0 symbol_1) (f64.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
