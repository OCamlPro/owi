  $ owi replay --replay-file simple.scfg simple.wat
  owi: [ERROR] unreachable
  [96]
  $ owi replay --replay-file simple.scfg simple.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 42 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 42 ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 42 ; i32.const 42 ]
  owi: [INFO] running instr : i32.eq (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable (executed 0 times)
  owi: [ERROR] unreachable
  [96]
