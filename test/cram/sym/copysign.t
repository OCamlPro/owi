float copysign:
  $ owi sym copysign_f32.wat --deterministic-result-order --no-value
  owi: [ERROR] Assert failure: (f32.eq (f32.mul -1. (f32.abs symbol_0))
                                (f32.copysign symbol_0 symbol_1))
  model {
    symbol symbol_0 f32
    symbol symbol_1 f32
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym copysign_f64.wat --deterministic-result-order --no-value
  owi: [ERROR] Assert failure: (f64.eq (f64.mul -1. (f64.abs symbol_0))
                                (f64.copysign symbol_0 symbol_1))
  model {
    symbol symbol_0 f64
    symbol symbol_1 f64
  }
  
  owi: [ERROR] Reached problem!
  [13]
