  $ owi wasm sym -w1 all_types.wat > all_types.scfg
  owi: [ERROR] Trap: unreachable
  owi: [ERROR] Reached problem!
  [13]
  $ owi wasm replay --replay-file all_types.scfg all_types.wat
  owi: [ERROR] unreachable
  [96]
