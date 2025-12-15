  $ owi run memarg.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 4 ]
  owi: [INFO] running instr : i32.const 99999999 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 99999999 ; i32.const 4 ]
  owi: [INFO] running instr : i32.store align=1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.load align=2 (executed 0 times)
  owi: [INFO] stack         : [ i32.const -16777216 ]
  owi: [INFO] running instr : drop (executed 0 times)
  $ owi wat2wasm memarg.wat
  $ owi run memarg.wasm -v
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 4 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 4 ]
  owi: [INFO] running instr : i32.const 99999999 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 99999999 ; i32.const 4 ]
  owi: [INFO] running instr : i32.store align=1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.load align=2 (executed 0 times)
  owi: [INFO] stack         : [ i32.const -16777216 ]
  owi: [INFO] running instr : drop (executed 0 times)
