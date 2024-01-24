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
