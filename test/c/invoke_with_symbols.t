  $ owi c --entry-point=f --invoke-with-symbols ./invoke_with_symbols.c
  Assert failure: (f32.ne (f32.convert_i32_s symbol_0) symbol_2)
  model {
    symbol symbol_0 i32 1
    symbol symbol_1 i32 0
    symbol symbol_2 f32 1.
    symbol symbol_3 f64 -1234.
  }
  Reached problem!
  [13]
