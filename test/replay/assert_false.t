  $ owi sym assert_false.wat -w1
  Assert failure: (i32.gt_u symbol_0 symbol_1)
  model {
    symbol symbol_0 i32 294734597
    symbol symbol_1 i32 -1853712862
  }
  Reached problem!
  [13]
  $ owi replay --replay-file assert_false.scfg assert_false.wat --debug
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : call 3
  calling func  : func f
  stack         : [  ]
  running instr : call 2
  stack         : [ i32.const 294734597 ]
  running instr : local.set 0
  stack         : [  ]
  running instr : call 2
  stack         : [ i32.const -1853712862 ]
  running instr : local.set 1
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 294734597 ]
  running instr : local.get 1
  stack         : [ i32.const -1853712862 ; i32.const 294734597 ]
  running instr : i32.lt_u
  stack         : [ i32.const 1 ]
  running instr : call 1
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i32.const 294734597 ]
  running instr : local.get 1
  stack         : [ i32.const -1853712862 ; i32.const 294734597 ]
  running instr : i32.gt_u
  stack         : [ i32.const 0 ]
  running instr : call 0
  Assertion failure was correctly reached

