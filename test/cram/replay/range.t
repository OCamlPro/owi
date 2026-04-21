  $ owi sym range.wat
  owi: [ERROR] Assert failure: (i32.lt_u symbol_1 299)
  model {
    symbol symbol_0 i32 16
    symbol symbol_1 i32 299
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi replay --replay-file range.scfg range.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2 (executed 0 times)
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 10 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 10 ]
  owi: [INFO] running instr : i32.const 20 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 20 ; i32.const 10 ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 16 ]
  owi: [INFO] running instr : local.set 0 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 16 ]
  owi: [INFO] running instr : i32.const 10 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 10 ; i32.const 16 ]
  owi: [INFO] running instr : i32.ge_u (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 16 ]
  owi: [INFO] running instr : i32.const 20 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 20 ; i32.const 16 ]
  owi: [INFO] running instr : i32.lt_u (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 200 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 200 ]
  owi: [INFO] running instr : i32.const 300 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 300 ; i32.const 200 ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 299 ]
  owi: [INFO] running instr : local.set 1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 299 ]
  owi: [INFO] running instr : i32.const 200 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 200 ; i32.const 299 ]
  owi: [INFO] running instr : i32.ge_u (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 299 ]
  owi: [INFO] running instr : i32.const 299 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 299 ; i32.const 299 ]
  owi: [INFO] running instr : i32.lt_u (executed 0 times)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] scopes : [symbol symbol_1 ; symbol symbol_0]
  Assertion failure was correctly reached!
