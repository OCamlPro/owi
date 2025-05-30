  $ owi c model_with_entry_point.c --model-with-entry-point -O0 --entry-point=main
  owi: [ERROR] Assert failure: (bool.eq symbol_0 3)
  model {
    symbol symbol_0 i32 2
    entry_point main
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi c model_with_entry_point.c --model-with-entry-point -O0 --entry-point=fun
  owi: [ERROR] Assert failure: (bool.eq symbol_0 2)
  model {
    symbol symbol_0 i32 1
    entry_point fun
  }
  owi: [ERROR] Reached problem!
  [13]

  $ owi c model_with_entry_point.c --model-with-entry-point -O0 --entry-point=fun --model-out-file=output.scfg -o output.wasm
  owi: [ERROR] Assert failure: (bool.eq symbol_0 2)
  owi: [ERROR] Reached problem!
  [13]

  $ owi replay output.wasm --replay-file=output.scfg
  Assertion failure was correctly reached!
