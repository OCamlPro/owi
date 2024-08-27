  $ owi wat2wasm globals.wat
  $ owi run globals.wasm --debug
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func anonymous
  stack        : [  ]
  running instr: global.get 0
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: global.set 1
  stack        : [  ]
  running instr: global.get 1
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  $ owi wasm2wat globals.wasm
  (module
    (type (sub final  (func)))
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
