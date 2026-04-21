(module
  (global $g1 i32 (i32.const 42))
  (global $g2 (mut i64) (i64.const 2))

  (func $start
    global.get $g1
    drop

    i64.const 42
    global.set $g2
    global.get $g2
    drop
  )

  (start $start)
)
