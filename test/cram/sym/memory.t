memory stuff:
  $ owi sym memory.wat --deterministic-result-order
  All OK!
  $ owi sym grow.wat --no-value --deterministic-result-order
  owi: [ERROR] Trap: out of bounds memory access
  model {
    symbol symbol_0 i32
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym store.wat --no-value --deterministic-result-order
  owi: [WARNING] Saw a symbolic address: symbol_0
  owi: [ERROR] Trap: out of bounds memory access
  model {
    symbol symbol_0 i32
  }
  
  owi: [ERROR] Reached problem!
  [13]
  $ owi sym memory2.wat --deterministic-result-order
  owi: [WARNING] Saw a symbolic address: symbol_0
  All OK!
