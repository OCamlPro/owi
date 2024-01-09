binop i32:
  $ dune exec owi -- sym binop_i32.wast --no-stop-at-failure
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
      (symbol_0 i32 (i32 1)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 -41)))
  Reached 5 problems!
binop i64:
  $ dune exec owi -- sym binop_i64.wast --no-stop-at-failure
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
      (symbol_0 i64 (i64 1)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 -39)))
  Reached 5 problems!
binop f32:
  $ dune exec owi -- sym binop_f32.wast --no-stop-at-failure
  owi: internal error, uncaught exception:
       File "src/symbolic_value.ml", line 436, characters 24-30: Assertion failed
       
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 -42.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 42.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f32 (f32 -0.000000)))
  [125]
binop f64:
  $ dune exec owi -- sym binop_f64.wast --no-stop-at-failure
  owi: internal error, uncaught exception:
       File "src/symbolic_value.ml", line 508, characters 24-30: Assertion failed
       
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -42.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 42.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 -0.000000)))
  [125]
