add binop:
  $ owi sym add_i32.wat --no-value --deterministic-result-order
  owi: [ERROR] Assert failure: (i32.le_s 0 (i32.add symbol_0 symbol_1))
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym add_i64.wat --no-value --deterministic-result-order
  owi: [ERROR] Assert failure: (i64.le_s 0 (i64.add symbol_0 symbol_1))
  model {
    symbol symbol_0 i64
    symbol symbol_1 i64
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym add_f32.wat --no-value --deterministic-result-order
  owi: [ERROR] Assert failure: (f32.eq (f32.add symbol_0 symbol_1)
                                (f32.add symbol_0 symbol_1))
  model {
    symbol symbol_0 f32
    symbol symbol_1 f32
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym add_f64.wat --no-value --deterministic-result-order
  owi: [ERROR] Assert failure: (f64.eq (f64.add symbol_0 symbol_1)
                                (f64.add symbol_0 symbol_1))
  model {
    symbol symbol_0 f64
    symbol symbol_1 f64
  }
  
  owi: [ERROR] Reached problem!
  [13]
