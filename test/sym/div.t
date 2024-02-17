div binop:
  $ dune exec owi -- sym div_i32.wat --no-value
  Trap: integer overflow
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached problem!
  found a bug while performing symbolic execution!
  [13]
  $ dune exec owi -- sym div_i64.wat --no-value
  Trap: integer overflow
  Model:
    (model
      (symbol_0 i64)
      (symbol_1 i64))
  Reached problem!
  found a bug while performing symbolic execution!
  [13]
  $ dune exec owi -- sym div_f32.wat --no-value
  Assert failure: (f32.eq (f32.div symbol_0 symbol_1) (f32.div symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f32)
      (symbol_1 f32))
  Reached problem!
  found a bug while performing symbolic execution!
  [13]
  $ dune exec owi -- sym div_f64.wat --no-value
  Assert failure: (f64.eq (f64.div symbol_0 symbol_1) (f64.div symbol_0 symbol_1))
  Model:
    (model
      (symbol_0 f64)
      (symbol_1 f64))
  Reached problem!
  found a bug while performing symbolic execution!
  [13]
