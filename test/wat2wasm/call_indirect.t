  $ owi wat2wasm call_indirect.wat
  $ owi run call_indirect.wasm --debug
  typechecking ...
  start function must have type [] -> []
  [32]
  $ owi wasm2wat call_indirect.wasm
  (module
    
    (type (sub final  (func (param i64) (result i64))))
    
    (type (sub final  (func)))
    (table 1 10 (ref null func))
    (func
      i64.const 1
      i32.const 0
      call_indirect 0  (param i64) (result i64)
      drop
    )
    (func (param i64) (result i64)
      local.get 0
      i64.const -1
      i64.add
    )
    (elem (table 0) (offset i32.const 0) (ref null func) (item ref.func 0))
    (start 1)
  )
