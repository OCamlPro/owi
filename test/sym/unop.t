unop i32:
  $ dune exec owi -- sym unop_i32.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 1))
      (symbol_1 i64 (i64 42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 1))
      (symbol_1 i64 (i64 0))
      (symbol_2 f32 (f32 0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 1))
      (symbol_1 i64 (i64 0))
      (symbol_2 f32 (f32 0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 512))
      (symbol_1 i64 (i64 0))
      (symbol_2 f32 (f32 -1074008064.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 1073741824))
      (symbol_1 i64 (i64 0))
      (symbol_2 f32 (f32 -2148007936.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 32))
      (symbol_1 i64 (i64 0))
      (symbol_2 f32 (f32 -2148007936.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32 (i32 8))
      (symbol_1 i64 (i64 0))
      (symbol_2 f32 (f32 -2148007936.000000)))
  Reached 8 problems!
unop i64:
  $ dune exec owi -- sym unop_i64.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_3 f64 (f64 0.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 42)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 4242)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_2 f32 (f32 0.000000))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 -2147483648)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 1))
      (symbol_2 f32 (f32 inf))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 -2147483648)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 131072))
      (symbol_2 f32 (f32 -inf))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 32768))
      (symbol_2 f32 (f32 -inf))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 -2147483648)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 i64 (i64 4294967296))
      (symbol_2 f32 (f32 -inf))
      (symbol_3 f64 (f64 0.000000))
      (symbol_1 i32 (i32 0)))
  Reached 9 problems!
unop f32:
  $ dune exec owi -- sym unop_f32.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 1109917696)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -2147483647))
      (symbol_0 f32 (f32 122.000977)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -2147479552))
      (symbol_0 f32 (f32 123.007812)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -2147479552))
      (symbol_0 f32 (f32 115.007812)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -2147479552))
      (symbol_0 f32 (f32 34.000000)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 2.000000))
      (symbol_2 i32 (i32 8))
      (symbol_0 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 1.000244))
      (symbol_2 i32 (i32 0))
      (symbol_0 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 1.031494))
      (symbol_2 i32 (i32 1))
      (symbol_0 f32 (f32 nan)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 -338460706455329128135018695364373065576360054171204343201037173805357995568625611429596369334345869072216864991017687935090688.000000))
      (symbol_2 i32 (i32 11821))
      (symbol_0 f32 (f32 nan))
      (symbol_3 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f64 (f64 nan))
      (symbol_2 i32 (i32 32768))
      (symbol_0 f32 (f32 nan))
      (symbol_3 i64 (i64 1)))
  Reached 10 problems!
unop f64:
  $ dune exec owi -- sym unop_f64.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
      (symbol_3 i64 (i64 4631107791820423168)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 123.000000))
      (symbol_3 i64 (i64 -9223372036854775807)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 123.004417))
      (symbol_3 i64 (i64 -4503049871556608)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 124.004417))
      (symbol_3 i64 (i64 -4503049871556608)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 f64 (f64 37.000504))
      (symbol_3 i64 (i64 9182840189964255232)))
  Trap: unreachable
  Model:
    (model
      (symbol_1 f32 (f32 2.000000))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 9182840189964255232)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 0))
      (symbol_1 f32 (f32 -627221245915981414400.000000))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 -9223371487098961920)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 1))
      (symbol_1 f32 (f32 2.002441))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 -9223371487098961920)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -2113927673))
      (symbol_1 f32 (f32 nan))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 0)))
  Trap: unreachable
  Model:
    (model
      (symbol_2 i32 (i32 -2113927673))
      (symbol_1 f32 (f32 nan))
      (symbol_0 f64 (f64 nan))
      (symbol_3 i64 (i64 1)))
  Reached 10 problems!
