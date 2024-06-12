unused local variables:
  $ owi opt local.wat > local.opt.wat
  $ cat local.opt.wat
  (module
    (func $f0 (local i32)
      i32.const 0
      local.set 0
    )
    (func $f1 (local i32)
      i32.const 0
      local.set 0
    )
    (func $f2 (local i32)
      i32.const 0
      local.set 0
    )
    (func $f3 (local i32)
      i32.const 0
      local.set 0
    )
    (func $f12 (local i32) (local i32)
      i32.const 0
      local.set 0
      i32.const 0
      local.set 1
    )
    (func $f23 (local i32) (local i32)
      i32.const 0
      local.set 0
      i32.const 0
      local.set 1
    )
    (func $f023 (local i32) (local i32) (local i32)
      i32.const 0
      local.set 0
      i32.const 0
      local.set 1
      i32.const 0
      local.set 2
    )
    (func $start
      call 0
      call 1
      call 2
      call 3
      call 4
      call 5
      call 6
    )
    (start 7)
  )
  $ owi run local.opt.wat
