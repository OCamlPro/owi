  $ owi sym range.wat
  Assert failure: (i32.lt_u symbol_1 299)
  model {
    symbol symbol_0 i32 16
    symbol symbol_1 i32 299
  }
  Reached problem!
  [13]
  $ owi replay --replay-file range.scfg range.wat --debug
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 2
  calling func : func start
  stack        : [  ]
  running instr: i32.const 10
  stack        : [ i32.const 10 ]
  running instr: i32.const 20
  stack        : [ i32.const 20 ; i32.const 10 ]
  running instr: call 0
  stack        : [ i32.const 16 ]
  running instr: local.set 0
  stack        : [  ]
  running instr: local.get 0
  stack        : [ i32.const 16 ]
  running instr: i32.const 10
  stack        : [ i32.const 10 ; i32.const 16 ]
  running instr: i32.ge_u
  stack        : [ i32.const 1 ]
  running instr: call 1
  Assertion failure was correctly reached
