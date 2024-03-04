memory stuff:
  $ dune exec owi -- sym memory.wat --deterministic-result-order
  All OK
  $ dune exec owi -- sym grow.wat --deterministic-result-order
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 (i32 1)))
  Reached problem!
  [13]
  $ dune exec owi -- sym store.wat --no-value --deterministic-result-order
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 i32))
  Reached problem!
  [13]
  $ dune exec owi -- sym memory2.wat --deterministic-result-order
  All OK
