entry_point:
  $ owi c entry_point.c --entry-point=fun
  Assert failure: (i32.lt_s symbol_0 19)
  model {
    symbol symbol_0 i32 19
  }
  Reached problem!
  [13]

  $ owi c entry_point.c
  clang failed: run with --debug to get the full error message
  [26]
