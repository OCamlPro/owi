  $ owi wat2wasm call_indirect.wat
  $ owi run call_indirect.wasm -v
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 0 ; i32.const 0 ]
  owi: [INFO] running instr : table.init 0 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : elem.drop 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i64.const 1
  owi: [INFO] stack         : [ i64.const 1 ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i64.const 1 ]
  owi: [INFO] running instr : call_indirect 0  (param i64) (result i64)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i64.const 1 ]
  owi: [INFO] running instr : i64.const -1
  owi: [INFO] stack         : [ i64.const -1 ; i64.const 1 ]
  owi: [INFO] running instr : i64.add
  owi: [INFO] stack         : [ i64.const 0 ]
  owi: [INFO] running instr : drop
  $ owi wasm2wat call_indirect.wasm
  (module
    (type (func (param i64) (result i64)))
    (type (func))
    (table 1 10 (ref null func))
    (func (param i64) (result i64)
      local.get 0
      i64.const -1
      i64.add
    )
    (func
      i64.const 1
      i32.const 0
      call_indirect 0  (param i64) (result i64)
      drop
    )
    (elem (table 0) (offset i32.const 0) (ref null func) (item ref.func 0))
    (start 1)
  )
