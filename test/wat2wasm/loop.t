  $ owi wat2wasm loop.wat
  $ owi run loop.wasm --debug
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 1
  calling func : func anonymous
  stack        : [  ]
  running instr: i32.const 3
  stack        : [ i32.const 3 ]
  running instr: call 0
  calling func : func anonymous
  stack        : [  ]
  running instr: i32.const 0
  stack        : [ i32.const 0 ]
  running instr: local.set 1
  stack        : [  ]
  running instr: (loop (result i64)
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
  stack        : [  ]
  running instr: local.get 1
  stack        : [ i32.const 0 ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ; i32.const 0 ]
  running instr: i32.add
  stack        : [ i32.const 1 ]
  running instr: local.tee 1
  stack        : [ i32.const 1 ]
  running instr: local.get 0
  stack        : [ i32.const 3 ; i32.const 1 ]
  running instr: i32.ne
  stack        : [ i32.const 1 ]
  running instr: (if
    (then
      br 1
    )
    (else
      nop
    )
  )
  stack        : [  ]
  running instr: br 1
  stack        : [  ]
  running instr: local.get 1
  stack        : [ i32.const 1 ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ; i32.const 1 ]
  running instr: i32.add
  stack        : [ i32.const 2 ]
  running instr: local.tee 1
  stack        : [ i32.const 2 ]
  running instr: local.get 0
  stack        : [ i32.const 3 ; i32.const 2 ]
  running instr: i32.ne
  stack        : [ i32.const 1 ]
  running instr: (if
    (then
      br 1
    )
    (else
      nop
    )
  )
  stack        : [  ]
  running instr: br 1
  stack        : [  ]
  running instr: local.get 1
  stack        : [ i32.const 2 ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ; i32.const 2 ]
  running instr: i32.add
  stack        : [ i32.const 3 ]
  running instr: local.tee 1
  stack        : [ i32.const 3 ]
  running instr: local.get 0
  stack        : [ i32.const 3 ; i32.const 3 ]
  running instr: i32.ne
  stack        : [ i32.const 0 ]
  running instr: (if
    (then
      br 1
    )
    (else
      nop
    )
  )
  stack        : [  ]
  running instr: nop
  stack        : [  ]
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  stack        : [ i64.const 42 ]
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  $ owi wasm2wat loop.wasm
  (module
    
    (type (sub final  (func (param i32) (result i64))))
    
    (type (sub final  (func (result i64))))
    
    (type (sub final  (func)))
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
