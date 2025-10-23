  $ owi wat2wasm loop.wat
  $ owi run loop.wasm -v
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 3
  owi: [INFO] stack         : [ i32.const 3 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : loop (result i64)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 0 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 1 ]
  owi: [INFO] running instr : i32.ne
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : br 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 2 ]
  owi: [INFO] running instr : i32.ne
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : br 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ i32.const 2 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 2 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 3 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ i32.const 3 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 3 ; i32.const 3 ]
  owi: [INFO] running instr : i32.ne
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : nop
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i64.const 42
  owi: [INFO] stack         : [ i64.const 42 ]
  owi: [INFO] running instr : drop
  $ owi wasm2wat loop.wasm
  (module
    (type (func (param i32) (result i64)))
    (type (func (result i64)))
    (type (func))
    (func (param i32) (result i64) (local i32)
      i32.const 0
      local.set 1
      (loop (result i64)
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        local.get 0
        i32.ne
        (if
          (then
            br 1
          )
          (else
            nop
          )
        )
        i64.const 42)
    )
    (func
      i32.const 3
      call 0
      drop
    )
    (start 1)
  )
