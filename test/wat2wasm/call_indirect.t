  $ owi wat2wasm call_indirect.wat
  $ owi run call_indirect.wasm --debug
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i32.const 0 ]
  running instr : i32.const 1
  stack         : [ i32.const 1 ; i32.const 0 ; i32.const 0 ]
  running instr : table.init 0 0
  stack         : [  ]
  running instr : elem.drop 0
  stack         : [  ]
  stack         : [  ]
  running instr : call 1
  calling func  : func anonymous
  stack         : [  ]
  running instr : i64.const 1
  stack         : [ i64.const 1 ]
  running instr : i32.const 0
  stack         : [ i32.const 0 ; i64.const 1 ]
  running instr : call_indirect 0  (param i64) (result i64)
  calling func  : func anonymous
  stack         : [  ]
  running instr : local.get 0
  stack         : [ i64.const 1 ]
  running instr : i64.const -1
  stack         : [ i64.const -1 ; i64.const 1 ]
  running instr : i64.add
  stack         : [ i64.const 0 ]
  stack         : [ i64.const 0 ]
  running instr : drop
  stack         : [  ]
  stack         : [  ]
  $ owi wasm2wat call_indirect.wasm
  (module
    (type (sub final  (func (param i64) (result i64))))
    (type (sub final  (func)))
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
