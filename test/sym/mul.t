mul binop:
  $ owi sym mul_i32.wat --no-value --deterministic-result-order
  Assert failure: (i32.ge_s (i32.mul symbol_0 symbol_1) 0)
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  Reached problem!
  [13]
  $ owi sym mul_i64.wat --no-value --deterministic-result-order
  Assert failure: (i64.ge_s (i64.mul symbol_0 symbol_1) 0)
  model {
    symbol symbol_0 i64
    symbol symbol_1 i64
  }
  Reached problem!
  [13]
  $ owi sym mul_f32.wat --no-value --deterministic-result-order
  Assert failure: (f32.eq (f32.mul symbol_0 symbol_1)
                   (f32.mul symbol_0 symbol_1))
  model {
    symbol symbol_0 f32
    symbol symbol_1 f32
  }
  Reached problem!
  [13]
  $ owi sym mul_f64.wat --no-value --deterministic-result-order
  Assert failure: (f64.eq (f64.mul symbol_0 symbol_1)
                   (f64.mul symbol_0 symbol_1))
  model {
    symbol symbol_0 f64
    symbol symbol_1 f64
  }
  Reached problem!
  [13]
