  $ owi iso a1.wat a2.wat
  All OK!
  $ owi iso a1.wat a3.wat
  owi: [ERROR] Assert failure: (bool.eq (i32.mul symbol_0 symbol_1)
                                (i32.add (i32.mul symbol_1 symbol_0) 1))
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  owi: [ERROR] Reached problem!
  [13]
