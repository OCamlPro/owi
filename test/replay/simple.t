  $ owi replay --replay-file simple.scfg simple.wat
  owi: [ERROR] unreachable
  [94]
  $ owi replay --replay-file simple.scfg simple.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] Parsing time : 243μs
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 188μs
  owi: [INFO] Validation time : 278μs
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 11.6μs
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32.const 42 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32.const 42 ; i32.const 42 ]
  owi: [INFO] running instr : i32.eq
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable
  owi: [ERROR] unreachable
  [94]
