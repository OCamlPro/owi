sub binop:
  $ dune exec owi -- sym sub_i32.wat
  Assert failure: (i32.ge symbol_0 (i32.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (i32 -2147483648))
      (symbol_1 (i32 2147483645)))
  Reached problem!
  [1]
  $ dune exec owi -- sym sub_i64.wat
  Assert failure: (i64.ge symbol_0 (i64.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (i64 -9223372036854775808))
      (symbol_1 (i64 9223372036854775805)))
  Reached problem!
  [1]
  $ dune exec owi -- sym sub_f32.wat
  Assert failure: (f32.eq (f32.sub symbol_0 symbol_1) (f32.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (f32 1.83670992316e-40))
      (symbol_1 (f32 nan)))
  Reached problem!
  [1]
  $ dune exec owi -- sym sub_f64.wat
  Assert failure: (f64.eq (f64.sub symbol_0 symbol_1) (f64.sub symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (f64 2.22507385851e-308))
      (symbol_1 (f64 nan)))
  Reached problem!
  [1]
