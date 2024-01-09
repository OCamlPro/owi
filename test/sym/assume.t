symbolic extern module (assume and assert test):
  $ dune exec owi -- sym assume.wast
  CHECK:
  (i32.not (i32.to_bool symbol_0))
  (i32.ge symbol_0 (i32 0))
  (i32.to_bool symbol_0)
  /CHECK OK
  All OK
