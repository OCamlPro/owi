symbolic extern module (assume and assert test):
  $ owi sym assume.wat --no-value --deterministic-result-order
  Trap: unreachable
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  Reached problem!
  [13]
