  $ owi wasm sym assert_false.wat -w1
  owi: [ERROR] Assert failure: (i32.lt_u symbol_1 symbol_0)
  model {
    symbol symbol_0 i32 1224779914
    symbol symbol_1 i32 -922707831
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi wasm replay --replay-file assert_false.scfg assert_false.wat
  Assertion failure was correctly reached!
