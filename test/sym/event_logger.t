memory stuff:
  $ owi sym memory.wat --deterministic-result-order --profile profile.json
  All OK
  $ ls profile.json
  profile.json
  $ owi sym grow.wat --no-value --deterministic-result-order --profile profile.json
  Trap: out of bounds memory access
  model {
    symbol symbol_0 i32
  }
  Reached problem!
  [13]

  $ ls profile.json
  profile.json
  $ owi sym store.wat --no-value --deterministic-result-order --profile profile.json
  Trap: out of bounds memory access
  model {
    symbol symbol_0 i32
  }
  Reached problem!
  [13]

  $ ls profile.json
  profile.json
  $ owi sym memory2.wat --deterministic-result-order --profile profile.json
  All OK

