set get tee simplification:
  $ owi opt tee.wat
  (module
    (func $start (local $x i32)
      i32.const 41
      local.tee 0
      i32.const 1
      i32.add
      drop
    )
    (start 0)
  )
