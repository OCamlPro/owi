global vars stuff:
  $ dune exec owi -- sym global.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 6)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -2147483642))
      (symbol_1 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -2147483642))
      (symbol_1 i64 (i64 4611686018427387904))
      (symbol_2 f32 (f32 14.000002)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -2147483642))
      (symbol_1 i64 (i64 4611686018427387904))
      (symbol_2 f32 (f32 0.000000))
      (symbol_3 f64 (f64 0.000000)))
  Reached 4 problems!
