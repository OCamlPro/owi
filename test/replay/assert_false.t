  $ owi sym assert_false.wat -w1
  owi: [ERROR] Assert failure: (i32.lt_u symbol_1 symbol_0)
  model {
    symbol symbol_0 i32 38077858
    symbol symbol_1 i32 38175397
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi replay --replay-file assert_false.scfg assert_false.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 3
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2
  owi: [INFO] stack         : [ i32.const 294734597 ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2
  owi: [INFO] stack         : [ i32.const -1853712862 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 294734597 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const -1853712862 ; i32.const 294734597 ]
  owi: [INFO] running instr : i32.lt_u
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 294734597 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const -1853712862 ; i32.const 294734597 ]
  owi: [INFO] running instr : i32.gt_u
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 0
  Assertion failure was correctly reached!
