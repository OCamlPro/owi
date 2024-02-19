memory stuff:
  $ dune exec owi -- sym memory.wat --no-stop-at-failure
  All OK
  $ dune exec owi -- sym grow.wat --no-stop-at-failure
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 (i32 1)))
  Reached problem!
  [13]
  $ dune exec owi -- sym store.wat
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 (i32 2146549760)))
  Reached problem!
  [13]
  $ dune exec owi -- sym memory2.wat
  All OK
