unop i32:
  $ dune exec owi -- sym unop_i32.wast --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 1))
      (symbol_1 f32 (f32 0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 1))
      (symbol_1 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 32))
      (symbol_1 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 128))
      (symbol_1 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 16384))
      (symbol_1 f32 (f32 nan)))
  Reached 6 problems!
unop i64:
  $ dune exec owi -- sym unop_i64.wast --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_1 f32 (f32 0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_1 f32 (f32 -36893492545465614336.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1099511627776))
      (symbol_1 f32 (f32 -36893492545465614336.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 4194304))
      (symbol_1 f32 (f32 -36893492545465614336.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 65536))
      (symbol_1 f32 (f32 -36893492545465614336.000000)))
  Reached 6 problems!
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
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 2.000000))
      (symbol_0 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 nan))
      (symbol_2 i32 (i32 0))
      (symbol_0 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 nan))
      (symbol_2 i32 (i32 1))
      (symbol_0 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 606188394992561843776909558182978781184.000000))
      (symbol_2 i32 (i32 33930))
      (symbol_0 f32 (f32 nan))
      (symbol_3 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 0.000000))
      (symbol_2 i32 (i32 150994944))
      (symbol_0 f32 (f32 nan))
      (symbol_3 i64 (i64 1)))
  Reached 7 problems!
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
  Trap: unreachable
  Model:
    (model
      (symbol_1 f32 (f32 2.000000))
      (symbol_0 f64 (f64 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 0))
      (symbol_1 f32 (f32 nan))
      (symbol_0 f64 (f64 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 1))
      (symbol_1 f32 (f32 nan))
      (symbol_0 f64 (f64 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -1342144375))
      (symbol_1 f32 (f32 -1.000490))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 2014380365))
      (symbol_1 f32 (f32 -1.017578))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 1)))
  Reached 7 problems!
