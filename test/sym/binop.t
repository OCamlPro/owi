binop i32:
  $ dune exec owi -- sym binop_i32.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -1073741923)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 2147483549)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 50)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 30)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -34)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -46)))
  Reached 9 problems!
binop i32 (2):
  $ dune exec owi -- sym binop_i32-2.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -65))
      (symbol_1 i32 (i32 -65)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 0))
      (symbol_1 i32 (i32 1)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 0))
      (symbol_1 i32 (i32 0))
      (symbol_3 i32 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 2))
      (symbol_2 i32 (i32 0))
      (symbol_1 i32 (i32 0))
      (symbol_3 i32 (i32 2)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -2147483618))
      (symbol_2 i32 (i32 0))
      (symbol_1 i32 (i32 0))
      (symbol_3 i32 (i32 -2147483648)))
  Reached 5 problems!
binop i64:
  $ dune exec owi -- sym binop_i64.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 -9223231299366420579)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 9223372036854775709)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 50)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 -42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 30)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 -34)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 -99)))
  Reached 9 problems!
binop i64 (2):
  $ dune exec owi -- sym binop_i64-2.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_1 i64 (i64 0))
      (symbol_2 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 i64 (i64 -1))
      (symbol_2 i64 (i64 -1)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 i64 (i64 -1))
      (symbol_2 i64 (i64 -2))
      (symbol_3 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 i64 (i64 -2))
      (symbol_0 i64 (i64 -9223372036854775808))
      (symbol_2 i64 (i64 -1))
      (symbol_3 i64 (i64 4611686018427387904)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 i64 (i64 -2))
      (symbol_0 i64 (i64 -9223372036854775804))
      (symbol_2 i64 (i64 -1))
      (symbol_3 i64 (i64 -4611686018427387904)))
  Reached 5 problems!
binop f32:
  $ dune exec owi -- sym binop_f32.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 -200.000015259)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 200.000015259)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 -42.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 42.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 1.12103877146e-44)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 -64.0058822632)))
  Reached 7 problems!
binop f64:
  $ dune exec owi -- sym binop_f64.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -200.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 200.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -42.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 42.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 4.94065645841e-323)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 1.48302076647e-309)))
  Reached 7 problems!
