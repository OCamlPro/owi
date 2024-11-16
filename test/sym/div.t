div binop:
  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1
  Trap: integer overflow
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Trap: integer divide by zero
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached 2 problems!
  [13]
  $ owi sym div_i64.wat --no-value --deterministic-result-order -w1
  Trap: integer overflow
  Model:
    (model
      (symbol_0 i64)
      (symbol_1 i64))
  Trap: integer divide by zero
  Model:
    (model
      (symbol_0 i64)
      (symbol_1 i64))
  Reached 2 problems!
  [13]
  $ owi sym div_f32.wat --no-value --deterministic-result-order -w1
  Assert failure: (f32.eq (f32.div symbol_0 symbol_1)
                   (f32.div symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym div_f64.wat --no-value --deterministic-result-order -w1
  Assert failure: (f64.eq (f64.div symbol_0 symbol_1)
                   (f64.div symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym div_zero.wat --deterministic-result-order -w1
  Trap: integer divide by zero
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 0)))
  Trap: integer divide by zero
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i32 0)))
  Trap: integer divide by zero
  Model:
    (model
      (symbol_0 (i32 2))
      (symbol_1 (i64 0)))
  Trap: integer divide by zero
  Model:
    (model
      (symbol_0 (i32 3))
      (symbol_1 (i64 0)))
  Reached 4 problems!
  [13]
