table stuff:
  $ dune exec owi -- sym table.wat --no-stop-at-failure -w1
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
