symbolic extern module (assume and assert test):
  $ owi sym assume.wat --no-value --deterministic-result-order
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  
  owi: [ERROR] Reached problem!
  [13]
