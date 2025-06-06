entry_point:
  $ owi rust entry_point.rs --entry-point=fun
  owi: [ERROR] Assert failure: (i32.le_s 4 symbol_0)
  model {
    symbol symbol_0 i32 2
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi rust entry_point.rs --entry-point=main
  owi: [ERROR] rustc failed: run with -vv to get the full error message if it was not displayed above
  [26]
