export:
  $ owi sym export.wat --entry-point=a
  All OK

  $ owi sym export.wat --entry-point=f
  Entry point f not found
  [26]

  $ owi sym export.wat --entry-point=fun
  Assert failure: (i32.lt_u symbol_0 19)
  model {
    symbol symbol_0 i32 19
  }
  Reached problem!
  [13]
