  $ owi wat2wasm mem.wat
  $ owi run mem.wasm
  $ owi wasm2wat mem.wasm
  (module
    (type (func))
    (type (func (param i32) (param i32)))
    (type (func (param i32) (param i64)))
    (type (func (param i32) (param f32)))
    (type (func (param i32) (param f64)))
    (memory 1)
    (func
      i32.const 32
      memory.grow 0
      drop
    )
    (func (param i32) (param i32)
      local.get 0
      local.get 1
      i32.store align=1
    )
    (func (param i32) (param i64)
      local.get 0
      local.get 1
      i64.store align=1
    )
    (func (param i32) (param f32)
      local.get 0
      local.get 1
      f32.store align=1
    )
    (func (param i32) (param f64)
      local.get 0
      local.get 1
      f64.store align=1
    )
    (func
      call 0
      memory.size 0
      drop
      i32.const 4
      i32.const 21
      call 1
      i32.const 9
      i64.const 42
      call 2
      i32.const 18
      f32.const 21.209_999_084_472_656
      call 3
      i32.const 23
      f64.const 42.420_000_000_000_002
      call 4
      i32.const 0
      i32.load align=1
      drop
      i32.const 4
      i32.load align=1
      drop
      i32.const 9
      i64.load align=1
      drop
      i32.const 18
      f32.load align=1
      drop
      i32.const 23
      f64.load align=1
      drop
    )
    (data (memory 0) (offset i32.const 0) "a")
    (start 5)
  )
