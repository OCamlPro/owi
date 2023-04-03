(module
  (func $start
    f64.const 42
    f32.demote_f64
    i32.const 0
    drop
    drop

    f32.const 42
    f64.promote_f32
    i32.const 0
    drop
    drop
  )

  (start $start)
)
