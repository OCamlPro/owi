(module
  (func $start
    (local $res i32)
    i32.const 0
    (loop (param i32) (result i32)
      (local.get $res)
      i32.const 2
      i32.add
      local.set $res
      (i32.le_s (i32.const 100) (local.get $res))
      br_if 0)
    drop
  )

  (start $start)
)
