div binop:
  $ dune exec owi -- sym div_i32.wat
  Trap: integer overflow
  Model:
    (model
      (symbol_0 (i32 -2147483648))
      (symbol_1 (i32 -1)))
  Reached problem!
  $ dune exec owi -- sym div_i64.wat
  Trap: integer overflow
  Model:
    (model
      (symbol_0 (i64 -9223372036854775808))
      (symbol_1 (i64 -1)))
  Reached problem!
  $ dune exec owi -- sym div_f32.wat
  Assert failure: (f32.eq (f32.div (f32 1.) symbol_0) (f32.div (f32 1.) symbol_1))
  Model:
    (model
      (symbol_0 (f32 0.))
      (symbol_1 (f32 -0.)))
  Reached problem!
