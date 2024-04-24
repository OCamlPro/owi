print symbolic:
  $ owi wasm2wat m.wasm
  (module
    (func
      i32.const 22
      i32.const 20
      call 0
      drop
    )
    (func (param i32) (param i32) (result i32)
      local.get 0
      local.get 1
      i32.add
    )
    (start 1)
  )
