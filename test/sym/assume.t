symbolic extern module (assume and assert test):
  $ dune exec owi -- sym assume.wast
  Assert failure: (i32.to_bool symbol_1)
  Model:
    (model
      (symbol_0 i32 (i32 1))
      (symbol_1 i32 (i32 0)))
  Reached problem!
