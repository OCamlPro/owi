  $ owi c alloc.c -o alloc.wasm -O0 --no-value
  owi: [ERROR] Assert failure: false
  model {
    symbol symbol_0 i32
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi wasm replay --replay-file alloc.scfg alloc.wasm --entry-point=main
  Assertion failure was correctly reached!
