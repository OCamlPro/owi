entry_point:
  $ owi zig entry_point.zig --entry-point=fun
  Assert failure: (i32.ge_s symbol_0 5)
  model {
    symbol symbol_0 i32 4
  }
  Reached problem!
  [13]

  $ owi zig entry_point.zig
  error: wasm-ld: entry symbol not defined (pass --no-entry to suppress): _start
  zig failed: run with --debug to get the full error message
  [26]
