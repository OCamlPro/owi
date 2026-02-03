  $ owi tinygo f.go --invoke-with-symbol --entry-point f
  owi: [WARNING] Saw a symbolic address: (i32.add 65520 (i32.shl symbol_0 2))
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 2
  }
  owi: [ERROR] Reached problem!
  [13]
