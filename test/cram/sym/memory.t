memory stuff:
  $ owi wasm sym memory.wat --deterministic-result-order
  All OK!
  $ owi wasm sym grow.wat --no-value --deterministic-result-order
  owi: [ERROR] Trap: out of bounds memory access
  model {
    symbol symbol_0 i32
  }
  
  owi: [ERROR] Reached problem!
  [13]

  $ owi wasm sym store.wat --no-value --deterministic-result-order
  owi: [ERROR] Trap: out of bounds memory access
  model {
    symbol symbol_0 i32
  }
  
  owi: [ERROR] Reached problem!
  [13]

  $ owi wasm sym memory2.wat --deterministic-result-order
  All OK!
