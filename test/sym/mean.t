symbolic extern module (assume and assert test):
  $ owi sym mean.wat -w1 --no-value --no-stop-at-failure --deterministic
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  owi: [ERROR] Reached problem!
  [13]
