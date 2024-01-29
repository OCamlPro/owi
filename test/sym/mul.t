mul binop:
  $ dune exec owi -- sym mul_i32.wat
  Assert failure: (i32.ge (i32.mul symbol_0 symbol_1) (i32 0))
  Model:
    (model
      (symbol_0 (i32 792711941))
      (symbol_1 (i32 229807418)))
  Reached problem!
  [1]
  $ dune exec owi -- sym mul_i64.wat
  Assert failure: (i64.ge (i64.mul symbol_0 symbol_1) (i64 0))
  Model:
    (model
      (symbol_0 (i64 3276315702285119495))
      (symbol_1 (i64 4152879772057883690)))
  Reached problem!
  [1]
  $ dune exec owi -- sym mul_f32.wat
  Assert failure: (f32.eq (f32.mul symbol_0 symbol_1) (f32.mul symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (f32 nan))
      (symbol_1 (f32 5.99266129048e-39)))
  Reached problem!
  [1]
  $ dune exec owi -- sym mul_f64.wat
  Assert failure: (f64.eq (f64.mul symbol_0 symbol_1) (f64.mul symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 (f64 -1.83569001651e-307))
      (symbol_1 (f64 nan)))
  Reached problem!
  [1]
