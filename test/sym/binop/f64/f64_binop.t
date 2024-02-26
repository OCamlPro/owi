f64:
  $ owi sym f64_add.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym f64_copysign.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym f64_div.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym f64_eq.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 1.)))
  Reached problem!
  [13]
  $ owi sym f64_ge.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 1.)))
  Reached problem!
  [13]
  $ owi sym f64_gt.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 2.)))
  Reached problem!
  [13]
  $ owi sym f64_le.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 1.)))
  Reached problem!
  [13]
  $ owi sym f64_lt.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64))
  Reached problem!
  [13]
  $ owi sym f64_max.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym f64_min.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym f64_mul.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
  $ owi sym f64_ne.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64))
  Reached problem!
  [13]
  $ owi sym f64_sub.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  [13]
