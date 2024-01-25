memory stuff:
  $ dune exec owi -- sym memory.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 1))
      (symbol_3 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 1))
      (symbol_3 (i64 1))
      (symbol_4 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 1))
      (symbol_3 (i64 1))
      (symbol_4 (i64 1))
      (symbol_5 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 1))
      (symbol_3 (i64 1))
      (symbol_4 (i64 1))
      (symbol_5 (i64 1))
      (symbol_6 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 1))
      (symbol_3 (i64 1))
      (symbol_4 (i64 1))
      (symbol_5 (i64 1))
      (symbol_6 (i64 1))
      (symbol_7 (f32 -0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 128))
      (symbol_1 (i32 32768))
      (symbol_2 (i32 1))
      (symbol_3 (i64 1))
      (symbol_4 (i64 1))
      (symbol_5 (i64 1))
      (symbol_6 (i64 1))
      (symbol_7 (f32 -2.00000023842))
      (symbol_8 (f64 -0.)))
  Reached 9 problems!
  $ dune exec owi -- sym grow.wat --no-stop-at-failure
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 (i32 1)))
  Reached 1 problems!
  $ dune exec owi -- sym store.wat
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 (i32 -11)))
  Reached problem!
