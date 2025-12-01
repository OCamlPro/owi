  $ owi run memarg.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
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
  owi: [INFO] running instr : i32.store 0 align=1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.load 0 align=2
  owi: [INFO] stack         : [ i32.const -16777216 ]
  owi: [INFO] running instr : drop
  $ owi wat2wasm memarg.wat
  $ owi run memarg.wasm -v
  owi: [INFO] typechecking ...
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
  owi: [INFO] running instr : i32.store 0 align=1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.load 0 align=2
  owi: [INFO] stack         : [ i32.const -16777216 ]
  owi: [INFO] running instr : drop
