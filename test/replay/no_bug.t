  $ owi replay --replay-file no_bug.scfg no_bug.wat
  All OK!
  $ owi replay --replay-file no_bug.scfg no_bug.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] Parsing time : 173μs
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 13.1μs
  owi: [INFO] Validation time : 87.8μs
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 9.16μs
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32.const 42 ]
  owi: [INFO] running instr : drop
  All OK!
