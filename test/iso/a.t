  $ owi iso a1.wat a2.wat
  Comparing a1.wat and a2.wat
  Common exports: mul
  Checking export mul
  All OK
  $ owi iso a1.wat a3.wat
  Comparing a1.wat and a3.wat
  Common exports: mul
  Checking export mul
  Assert failure: (bool.eq (i32.mul symbol_0 symbol_1)
                   (i32.add (i32.mul symbol_1 symbol_0) 1))
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  Reached problem!
  [13]
