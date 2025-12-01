  $ owi wat2wasm rec.wat
  $ owi run rec.wasm
  $ owi wasm2wat rec.wasm
  (module
    (type (func (param i32) (result i32)))
    (type (func))
    (memory 1)
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
      i32.load 0 align=1
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
          i32.store 0 align=1
        )
      )
      local.get 0
      i32.const 4
      i32.mul
      i32.load 0 align=1
      return
    )
    (func
      i32.const 4
      call 0
      drop
    )
    (start 1)
  )

