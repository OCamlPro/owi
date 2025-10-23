  $ owi run memarg.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] Parsing time : 1.61ms
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 15.8μs
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 4
  owi: [INFO] stack         : [ i32.const 4 ]
  owi: [INFO] running instr : i32.const 99999999
  owi: [INFO] stack         : [ i32.const 99999999 ; i32.const 4 ]
  owi: [INFO] running instr : i32.store align=1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.load align=2
  owi: [INFO] stack         : [ i32.const -16777216 ]
  owi: [INFO] running instr : drop
  $ owi wat2wasm memarg.wat
  $ owi run memarg.wasm -v
  owi: [INFO] Parsing time : 162μs
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 20.1μs
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 4
  owi: [INFO] stack         : [ i32.const 4 ]
  owi: [INFO] running instr : i32.const 99999999
  owi: [INFO] stack         : [ i32.const 99999999 ; i32.const 4 ]
  owi: [INFO] running instr : i32.store align=1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.load align=2
  owi: [INFO] stack         : [ i32.const -16777216 ]
  owi: [INFO] running instr : drop
