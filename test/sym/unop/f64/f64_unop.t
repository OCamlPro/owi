f64:
  $ owi sym f64_abs.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_ceil.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_convert_i32_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_convert_i32_u.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_convert_i64_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i64 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_convert_i64_u.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i64 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_floor.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_nearest.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_neg.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_promote_f32.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym f64_reinterpret_i64.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i64
  }
  owi: [ERROR] Reached problem!
  [13]
$ owi sym f64_sqrt.wat --no-stop-at-failure --no-value
  $ owi sym f64_trunc.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
