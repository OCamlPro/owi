f32:
  $ owi sym f32_abs.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_abs.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_ceil.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_convert_i32_s.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0)))
  Reached problem!
  [13]
  $ owi sym f32_convert_i32_u.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0)))
  Reached problem!
  [13]
  $ owi sym f32_convert_i64_s.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 0)))
  Reached problem!
  [13]
  $ owi sym f32_convert_i64_u.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 0)))
  Reached problem!
  [13]
  $ owi sym f32_demote_f64.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64))
  Reached problem!
  [13]
  $ owi sym f32_floor.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_nearest.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_neg.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_reinterpret_i32.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32))
  Reached problem!
  [13]
$ owi sym f32_sqrt.wat --no-stop-at-failure --no-value
  $ owi sym f32_trunc.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
