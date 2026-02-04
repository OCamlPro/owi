  $ owi tinygo f.go --invoke-with-symbol --entry-point f 2>&1 | grep -v "WARNING"
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 2
  }
  owi: [ERROR] Reached problem!
