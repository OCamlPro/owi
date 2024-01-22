(module

  (global $g i32 (i32.const 0))

  (func $start
    call $const
    call $local_global
  )

  (func $const
    i32.const 42

    i32.const 0
    drop
    i64.const 0
    drop
    f32.const 0
    drop
    f64.const 0
    drop

    drop
  )

  (func $local_global
    (local $l i32)
    i32.const 42

    local.get $l
    drop
    global.get $g
    drop

    drop
  )

  (start $start)
)
