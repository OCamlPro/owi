(module
  (func $start
    (local $x i32)
    i32.const 41
    local.set $x
    local.get $x
    i32.const 1
    i32.add
    drop
  )
  (start $start)
)
