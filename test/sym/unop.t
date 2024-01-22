unop i32:
  $ dune exec owi -- sym unop_i32.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i64 42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i64 0))
      (symbol_2 (f32 5.88545355016e-44)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1))
      (symbol_1 (i64 0))
      (symbol_2 (f32 2.9147008058e-42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 512))
      (symbol_1 (i64 0))
      (symbol_2 (f32 -1074008064.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 1073741824))
      (symbol_1 (i64 0))
      (symbol_2 (f32 -2148007936.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 32))
      (symbol_1 (i64 0))
      (symbol_2 (f32 -2148007936.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 8))
      (symbol_1 (i64 0))
      (symbol_2 (f32 -2148007936.)))
  Reached 8 problems!
unop i64:
  $ dune exec owi -- sym unop_i64.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 1))
      (symbol_3 (f64 2.07507571253e-322)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 1))
      (symbol_1 (i32 42))
      (symbol_3 (f64 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 1))
      (symbol_1 (i32 4242))
      (symbol_3 (f64 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 1))
      (symbol_1 (i32 -2147483648))
      (symbol_2 (f32 1.17563784379e-38))
      (symbol_3 (f64 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 1))
      (symbol_1 (i32 -2147483648))
      (symbol_2 (f32 infinity))
      (symbol_3 (f64 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 131072))
      (symbol_1 (i32 0))
      (symbol_2 (f32 neg_infinity))
      (symbol_3 (f64 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 32768))
      (symbol_1 (i32 -2147483648))
      (symbol_2 (f32 neg_infinity))
      (symbol_3 (f64 0.)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i64 4294967296))
      (symbol_1 (i32 0))
      (symbol_2 (f32 neg_infinity))
      (symbol_3 (f64 0.)))
  Reached 9 problems!
unop f32:
  $ dune exec owi -- sym unop_f32.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_2 (i32 1109917696)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 122.000976562))
      (symbol_2 (i32 -2147483647)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 123.0078125))
      (symbol_2 (i32 -2147479552)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 115.0078125))
      (symbol_2 (i32 -2147479552)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 34.))
      (symbol_2 (i32 -2147479552)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f64 1.9999999404))
      (symbol_2 (i32 8)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f64 1.00024442375))
      (symbol_2 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f64 1.03149430454))
      (symbol_2 (i32 1)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f64 -3.38460706455e+125))
      (symbol_2 (i32 11821))
      (symbol_3 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f64 -1.02319338918))
      (symbol_2 (i32 8388624))
      (symbol_3 (i64 1)))
  Reached 10 problems!
unop f64:
  $ dune exec owi -- sym unop_f64.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_3 (i64 4631107791820423168)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 123.))
      (symbol_3 (i64 -9223372036854775807)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 123.004417419))
      (symbol_3 (i64 -4503049871556608)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 124.004417419))
      (symbol_3 (i64 -4503049871556608)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 37.00050354))
      (symbol_3 (i64 9182840189964255232)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 nan))
      (symbol_1 (f32 2.))
      (symbol_3 (i64 9182840189964255232)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 nan))
      (symbol_1 (f32 -6.27221245916e+20))
      (symbol_2 (i32 0))
      (symbol_3 (i64 -9223371487098961920)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 nan))
      (symbol_1 (f32 2.00244140625))
      (symbol_2 (i32 1))
      (symbol_3 (i64 -9223371487098961920)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 nan))
      (symbol_1 (f32 nan))
      (symbol_2 (i32 -2113927673))
      (symbol_3 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (f64 nan))
      (symbol_1 (f32 nan))
      (symbol_2 (i32 -2113927673))
      (symbol_3 (i64 1)))
  Reached 10 problems!
