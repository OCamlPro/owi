export:
  $ owi sym export.wat --entry-point=a
  All OK!

  $ owi sym export.wat --entry-point=f
  owi: [ERROR] Entry point f not found
  [26]

  $ owi sym export.wat --entry-point=fun
  owi: [ERROR] Assert failure: (i32.lt_u symbol_0 19)
  model {
    symbol symbol_0 i32 19
  }
  owi: [ERROR] Reached problem!
  [13]
