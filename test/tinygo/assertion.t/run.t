  $ owi tinygo check.go --invoke-with-symbol --entry-point check
  owi: [ERROR] Assert failure: (bool.eq
                                (i32.add (i32.and symbol_0 symbol_1)
                                 (i32.shr_s (i32.xor symbol_0 symbol_1) 1))
                                (i32.div_s (i32.add symbol_0 symbol_1) 2))
  model {
    symbol symbol_0 i32 -2147483648
    symbol symbol_1 i32 -2147483646
  }
  owi: [ERROR] Reached problem!
  [13]
