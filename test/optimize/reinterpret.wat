(module
  (func $start
    i32.const 0
    f32.reinterpret_i32
    i32.const 0
    drop
    drop

    i64.const 0
    f64.reinterpret_i64
    i32.const 0
    drop
    drop

    f32.const 0
    i32.reinterpret_f32
    i32.const 0
    drop
    drop

    f64.const 0
    i64.reinterpret_f64
    i32.const 0
    drop
    drop
  )

  (start $start)
)
