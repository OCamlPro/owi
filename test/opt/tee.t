set get tee simplification:
  $ owi opt tee.wat > tee.opt.wat
  $ cat tee.opt.wat
  (module
    (type (func))
    (func $start (local $x i32)
      i32.const 41
      local.tee 0
      i32.const 1
      i32.add
      drop
    )
    (start 0)
  )
  $ owi run tee.opt.wat
