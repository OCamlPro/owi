i64:
  $ owi sym i64_clz.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 i64
  }
  Reached problem!
  [13]
$ owi sym i64_ctz.wat --no-stop-at-failure --no-value
  $ owi sym i64_eqz.wat --no-stop-at-failure
  Trap: unreachable
  model {
    symbol symbol_0 i64 0
  }
  Reached problem!
  [13]
  $ owi sym i64_extend_i32_s.wat --no-stop-at-failure
  Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  Reached problem!
  [13]
  $ owi sym i64_extend_i32_u.wat --no-stop-at-failure
  Trap: unreachable
  model {
    symbol symbol_0 i32 1
  }
  Reached problem!
  [13]
  $ owi sym i64_extend8_s.wat --no-stop-at-failure
  Trap: unreachable
  model {
    symbol symbol_0 i64 1
  }
  Reached problem!
  [13]
  $ owi sym i64_extend16_s.wat --no-stop-at-failure
  Trap: unreachable
  model {
    symbol symbol_0 i64 1
  }
  Reached problem!
  [13]
  $ owi sym i64_extend32_s.wat --no-stop-at-failure
  Trap: unreachable
  model {
    symbol symbol_0 i64 1
  }
  Reached problem!
  [13]
$ owi sym i64_popcnt.wat --no-stop-at-failure --no-value
  $ owi sym i64_reinterpret_f32.wat --no-stop-at-failure
  All OK
  $ owi sym i64_trunc_f32_s_ge_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_s_gt_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_s_le_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_s_lt_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_u_ge_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_u_gt_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_u_le_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f32_u_lt_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f32
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_s_ge_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_s_gt_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_s_le_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_s_lt_s.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_u_ge_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_u_gt_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_u_le_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
  $ owi sym i64_trunc_f64_u_lt_u.wat --no-stop-at-failure --no-value
  Trap: unreachable
  model {
    symbol symbol_0 f64
  }
  Reached problem!
  [13]
