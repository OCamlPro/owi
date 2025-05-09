  $ owi c scopes.c -O0 --no-value
  owi: [ERROR] Assert failure: false
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32 "scope 1"
    scope "scope 2" {
      symbol symbol_2 i32
      symbol symbol_3 i32
      symbol symbol_4 i32
    }
    scope "scope 3" {
      symbol symbol_5 i32 "scope 3.1"
      scope "scope 3.2" {
        symbol symbol_6 i32
        symbol symbol_7 i32
      }
    }
    symbol symbol_8 i32 aaa
    symbol symbol_9 i32 aaa
  }
  owi: [ERROR] Reached problem!
  [13]
