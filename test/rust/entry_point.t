entry_point:
  $ owi rust entry_point.rs --entry-point=fun
  Assert failure: (i32.ge_s symbol_0 4)
  model {
    symbol symbol_0 i32 2
  }
  Reached problem!
  [13]

  $ owi rust entry_point.rs --entry-point=main
  rustc failed: run with --debug to get the full error message
  [26]
