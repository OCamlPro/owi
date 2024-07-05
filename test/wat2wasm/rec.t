  $ owi wat2wasm rec.wat
  $ owi run rec.wasm
  start function must have type [] -> []
  [32]
  $ owi wasm2wat rec.wasm
  (module
    
    (type (sub final  (func (param i32) (result i32))))
    
    (type (sub final  (func)))
    (memory 1)
    (func
      i32.const 4
      call 0
      drop
    )
    (func (param i32) (result i32)
      local.get 0
      i32.const 0
      i32.lt_s
      (if
        (then
          unreachable
        )
      )
      local.get 0
      i32.const 2
      i32.lt_s
      (if
        (then
          local.get 0
          return
        )
      )
      i32.const 4
      local.get 0
      i32.mul
      i32.load align=1
      i32.eqz
      (if
        (then
          local.get 0
          i32.const 4
          i32.mul
          local.get 0
          i32.const 1
          i32.sub
          call 0
          local.get 0
          i32.const 2
          i32.sub
          call 0
          i32.add
          i32.store align=1
        )
      )
      local.get 0
      i32.const 4
      i32.mul
      i32.load align=1
      return
    )
    (start 1)
  )

