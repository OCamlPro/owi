div binop:
  $ owi sym div_i32.wat --no-value --deterministic-result-order -w1
  owi: [ERROR] Trap: integer overflow
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  owi: [ERROR] Reached 2 problems!
  [13]
  $ owi sym div_i64.wat --no-value --deterministic-result-order -w1
  owi: [ERROR] Trap: integer overflow
  model {
    symbol symbol_0 i64
    symbol symbol_1 i64
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i64
    symbol symbol_1 i64
  }
  owi: [ERROR] Reached 2 problems!
  [13]
  $ owi sym div_f32.wat --no-value --deterministic-result-order -w1
  owi: [ERROR] Assert failure: (f32.eq (f32.div_s symbol_0 symbol_1)
                                (f32.div_s symbol_0 symbol_1))
  model {
    symbol symbol_0 f32
    symbol symbol_1 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym div_f64.wat --no-value --deterministic-result-order -w1
  owi: [ERROR] Assert failure: (f64.eq (f64.div_s symbol_0 symbol_1)
                                (f64.div_s symbol_0 symbol_1))
  model {
    symbol symbol_0 f64
    symbol symbol_1 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym div_zero.wat --deterministic-result-order -w1
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 1
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 2
    symbol symbol_1 i64 0
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 3
    symbol symbol_1 i64 0
  }
  owi: [ERROR] Reached 4 problems!
  [13]
