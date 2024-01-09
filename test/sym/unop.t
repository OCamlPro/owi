unop i32:
  $ dune exec owi -- sym unop_i32.wast --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0)))
  Reached 1 problems!
unop i64:
  $ dune exec owi -- sym unop_i64.wast --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 0)))
  Reached 1 problems!
unop f32:
  $ dune exec owi -- sym unop_f32.wast --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 42.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 -0.000000)))
  Reached 2 problems!
unop f64:
  $ dune exec owi -- sym unop_f64.wast --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 42.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -0.000000)))
  Reached 2 problems!
