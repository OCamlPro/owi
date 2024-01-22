memory stuff:
  $ dune exec owi -- sym memory.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 6)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 134)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0))
      (symbol_1 i32 (i32 8)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0))
      (symbol_1 i32 (i32 32774)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 6))
      (symbol_1 i32 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 -2147483642))
      (symbol_1 i32 (i32 0))
      (symbol_3 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_4 i64 (i64 0))
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 -2147483642))
      (symbol_1 i32 (i32 0))
      (symbol_3 i64 (i64 64)))
  Trap: unreachable
  Model:
    (model
      (symbol_4 i64 (i64 16384))
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 -2147483642))
      (symbol_1 i32 (i32 0))
      (symbol_5 i64 (i64 0))
      (symbol_3 i64 (i64 64)))
  Trap: unreachable
  Model:
    (model
      (symbol_4 i64 (i64 16384))
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 -2147483642))
      (symbol_1 i32 (i32 0))
      (symbol_6 i64 (i64 0))
      (symbol_5 i64 (i64 1073741824))
      (symbol_3 i64 (i64 64)))
  Trap: unreachable
  Model:
    (model
      (symbol_4 i64 (i64 16384))
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 -2147483642))
      (symbol_1 i32 (i32 0))
      (symbol_6 i64 (i64 4611686018427387904))
      (symbol_7 f32 (f32 12.))
      (symbol_5 i64 (i64 1073741824))
      (symbol_3 i64 (i64 64)))
  Trap: unreachable
  Model:
    (model
      (symbol_4 i64 (i64 16384))
      (symbol_0 i32 (i32 0))
      (symbol_2 i32 (i32 -2147483642))
      (symbol_1 i32 (i32 0))
      (symbol_8 f64 (f64 0.))
      (symbol_6 i64 (i64 4611686018427387904))
      (symbol_7 f32 (f32 5.87747175411e-39))
      (symbol_5 i64 (i64 1073741824))
      (symbol_3 i64 (i64 64)))
  Reached 11 problems!
  $ dune exec owi -- sym grow.wat --no-stop-at-failure
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 i32 (i32 1)))
  Reached 1 problems!
