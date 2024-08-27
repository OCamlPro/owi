  $ owi wasm2wat locals.wasm
  (module
    (type (sub final  (func (param i32) (param i32) (param i32))))
    (type (sub final  (func)))
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 3
      local.get 4
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 3
      i32.const 0
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 4
      i32.const 0
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 0
      i32.const 0
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 1
      i32.const 0
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 2
      i32.const 0
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 4
      local.get 0
      i32.add
      drop
    )
    (func (param i32) (param i32) (param i32) (local i32) (local i32)
      local.get 0
      local.get 1
      i32.add
      drop
    )
    (func
      i32.const 0
      i32.const 1
      i32.const 2
      call 0
      i32.const 0
      i32.const 1
      i32.const 2
      call 1
      i32.const 0
      i32.const 1
      i32.const 2
      call 2
      i32.const 0
      i32.const 1
      i32.const 2
      call 3
      i32.const 0
      i32.const 1
      i32.const 2
      call 4
      i32.const 0
      i32.const 1
      i32.const 2
      call 5
      i32.const 0
      i32.const 1
      i32.const 2
      call 6
      i32.const 0
      i32.const 1
      i32.const 2
      call 7
    )
    (start 8)
  )
