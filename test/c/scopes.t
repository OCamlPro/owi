  $ owi c scopes.c -O0 --no-value
  owi: [ERROR] Assert failure: false
  model {
    symbol symbol_0 i32
    symbol "scope 1" symbol_1 i32
    scope "scope 2" {
      symbol symbol_2 i32
      symbol symbol_3 i32
      symbol symbol_4 i32
    }
  }
  owi: [ERROR] Reached problem!
  [13]
