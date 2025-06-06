entry_point:
  $ owi zig entry_point.zig --entry-point=fun
  owi: [ERROR] Assert failure: (i32.le_s 5 symbol_0)
  model {
    symbol symbol_0 i32 4
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi zig entry_point.zig
  owi: [ERROR] zig failed: run with -vv to get the full error message if it was not displayed above
  [26]
