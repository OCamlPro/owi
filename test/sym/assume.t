symbolic extern module (assume and assert test):
  $ dune exec owi -- sym assume.wat --no-stop-at-failure --no-value
  Trap: unreachable
  Model:
    (model
      (symbol_0 i32)
      (symbol_1 i32))
  found a bug while performing symbolic execution!
  [13]
