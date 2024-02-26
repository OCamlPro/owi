f32:
  $ owi sym f32_add.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym f32_copysign.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym f32_div.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym f32_eq.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 1.)))
  Reached problem!
  [13]
  $ owi sym f32_ge.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 1.)))
  Reached problem!
  [13]
  $ owi sym f32_gt.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_le.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 1.)))
  Reached problem!
  [13]
  $ owi sym f32_lt.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_max.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym f32_min.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym f32_mul.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
  $ owi sym f32_ne.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32))
  Reached problem!
  [13]
  $ owi sym f32_sub.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  [13]
