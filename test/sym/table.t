table stuff:
  $ dune exec owi -- sym table.wat --deterministic-result-order
  Trap: undefined element
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 -2147483648)))
  Trap: unreachable
  Model:
    (model
      (symbol_0 (i32 0))
      (symbol_1 (i32 0)))
  Reached 2 problems!
  [13]
