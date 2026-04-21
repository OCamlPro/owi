test that we can emit to a file: 
  $ owi wasm2wat m.wasm --emit-file
  $ cat m.wat
  (module
    (type (func (param i32) (param i32) (result i32)))
    (type (func))
    (func (param i32) (param i32) (result i32)
      local.get 0
      local.get 1
      i32.add
    )
    (func
      i32.const 22
      i32.const 20
      call 0
      drop
    )
    (start 1)
  )
