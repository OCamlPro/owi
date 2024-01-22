(module
  (func $start
    i32.const 42
    f32.convert_i32_s
    i32.const 0
    drop
    drop

    i32.const 42
    f32.convert_i32_u
    i32.const 0
    drop
    drop

    i64.const 42
    f32.convert_i64_s
    i32.const 0
    drop
    drop

    i64.const 42
    f32.convert_i64_u
    i32.const 0
    drop
    drop

    i32.const 42
    f64.convert_i32_s
    i32.const 0
    drop
    drop

    i32.const 42
    f64.convert_i32_u
    i32.const 0
    drop
    drop

    i64.const 42
    f64.convert_i64_s
    i32.const 0
    drop
    drop

    i64.const 42
    f64.convert_i64_u
    i32.const 0
    drop
    drop
  )

  (start $start)
)
