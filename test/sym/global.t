global vars stuff:
  $ dune exec owi -- sym global.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i64 1))
      (symbol_2 (f32 -0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i64 1))
      (symbol_2 (f32 -2.35098926216e-38))
      (symbol_3 (f64 -0.)))
  Reached 4 problems!
