  $ owi wat2wasm globals.wat
  $ owi run globals.wasm -v
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : global.get 0 (executed 0 times)
  owi: [INFO] stack         : [ i32.const 42 ]
  owi: [INFO] running instr : drop (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i64.const 42 (executed 0 times)
  owi: [INFO] stack         : [ i64.const 42 ]
  owi: [INFO] running instr : global.set 1 (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : global.get 1 (executed 0 times)
  owi: [INFO] stack         : [ i64.const 42 ]
  owi: [INFO] running instr : drop (executed 0 times)
  $ owi wasm2wat globals.wasm
  (module
    (type (func))
    (global i32 i32.const 42)
    (global (mut i64) i64.const 2)
    (func
      global.get 0
      drop
      i64.const 42
      global.set 1
      global.get 1
      drop
    )
    (start 0)
  )
