  $ owi sym model_out_file.wat --model-out-file=model_out_file.scfg
  owi: [ERROR] Assert failure: (i32.le_u 20 symbol_0)
  owi: [ERROR] Reached problem!
  [13]

  $ owi replay model_out_file.wat --replay-file=model_out_file.scfg
  Assertion failure was correctly reached!
