entry_point:
  $ owi c++ entry_point.cpp --entry-point=fun
  owi: [ERROR] Assert failure: (i32.lt_s symbol_0 19)
  model {
    symbol symbol_0 i32 19
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi c++ entry_point.cpp
  owi: [ERROR] wasm-ld failed: run with -vv to get the full error message if it was not displayed above
  [26]
