global vars stuff:
  $ dune exec owi -- sym global.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 1)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 -2147483647))
      (symbol_2 (i64 1))
      (symbol_3 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 -2147483647))
      (symbol_2 (i64 -9223372036854775807))
      (symbol_3 (i64 0))
      (symbol_4 (f32 -2.03125))
      (symbol_5 (f32 1.19386159018e-38)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 -2147483647))
      (symbol_2 (i64 -9223372036854775807))
      (symbol_3 (i64 0))
      (symbol_4 (f32 2.12513160706))
      (symbol_5 (f32 1.24904037968e-38))
      (symbol_6 (f64 2.98443202363e-154))
      (symbol_7 (f64 -3.69206570797e+19)))
  Reached 4 problems!
