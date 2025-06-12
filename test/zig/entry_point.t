entry_point:
  $ owi zig entry_point.zig --entry-point=fun
  owi: [ERROR] Assert failure: (i32.le_s 5 symbol_0)
  model {
    symbol symbol_0 i32 4
    entry_point fun
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi zig entry_point.zig
  owi: [ERROR] zig failed: run with --debug to get the full error message
  [26]
