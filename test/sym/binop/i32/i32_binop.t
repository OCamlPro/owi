i32:
  $ owi sym i32_add.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 -1
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_and.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_div_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_div_u.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_eq.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_ge_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1073741824
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_gt_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 2
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_le_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 -2147483646
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_lt_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_mul.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_ne.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_or.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_rem_s.wat --no-stop-at-failure --deterministic-result-order
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 -1
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached 2 problems!
  [13]
  $ owi sym i32_rem_u.wat --no-stop-at-failure --deterministic-result-order
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  owi: [ERROR] Trap: integer divide by zero
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached 2 problems!
  [13]
  $ owi sym i32_rotl.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_rotr.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_shl.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_shr_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_shr_u.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_sub.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_xor.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 -67108865
    symbol symbol_1 i32 67108864
  }
  owi: [ERROR] Reached problem!
  [13]
