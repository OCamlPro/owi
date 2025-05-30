  $ owi c -O0 --entry-point=f --invoke-with-symbols ./invoke_with_symbols.c
  owi: [ERROR] Assert failure: (f32.ne
                                (f32.reinterpret_int
                                 (i32.reinterpret_float symbol_2))
                                (f32.convert_i32_s symbol_0))
  model {
    symbol symbol_0 i32 1
    symbol symbol_1 i64 0
    symbol symbol_2 f32 1.
    symbol symbol_3 f64 -1234.
  }
  owi: [ERROR] Reached problem!
  [13]
