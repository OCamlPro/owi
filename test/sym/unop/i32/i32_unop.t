i32:
  $ owi sym i32_clz.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32
  }
  owi: [ERROR] Reached problem!
  [13]
$ owi sym i32_ctz.wat --no-stop-at-failure --no-value
  $ owi sym i32_eqz.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_extend8_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_extend16_s.wat --no-stop-at-failure
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  owi: [ERROR] Reached problem!
  [13]
$ owi sym i32_popcnt.wat --no-stop-at-failure --no-value
  $ owi sym i32_reinterpret_f32.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_s_ge_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_s_gt_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_s_le_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_s_lt_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_u_ge_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_u_gt_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_u_le_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f32_u_lt_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_s_ge_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_s_gt_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_s_le_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_s_lt_s.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_u_ge_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_u_gt_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_u_le_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym i32_trunc_f64_u_lt_u.wat --no-stop-at-failure --no-value
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  owi: [ERROR] Reached problem!
  [13]
