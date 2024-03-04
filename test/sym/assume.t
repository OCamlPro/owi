symbolic extern module (assume and assert test):
  $ dune exec owi -- sym assume.wat --no-value --deterministic-result-order
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  Reached problem!
  [13]
