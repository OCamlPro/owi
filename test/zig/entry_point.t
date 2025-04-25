entry_point:
  $ owi zig entry_point.zig --entry-point=fun
  owi: [ERROR] Assert failure: (i32.ge_s symbol_0 5)
  model {
    symbol symbol_0 i32 4
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi zig entry_point.zig
  owi: [ERROR] zig failed: run with --debug to get the full error message
  [26]
