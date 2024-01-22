(module
  (func $trunc
    f32.const 42
    i32.trunc_f32_s
    i32.const 0
    drop
    drop

    f32.const 42
    i32.trunc_f32_u
    i32.const 0
    drop
    drop

    f64.const 42
    i32.trunc_f64_s
    i32.const 0
    drop
    drop

    f64.const 42
    i32.trunc_f64_u
    i32.const 0
    drop
    drop

    f32.const 42
    i64.trunc_f32_s
    i32.const 0
    drop
    drop

    f32.const 42
    i64.trunc_f32_u
    i32.const 0
    drop
    drop

    f64.const 42
    i64.trunc_f64_s
    i32.const 0
    drop
    drop

    f64.const 42
    i64.trunc_f64_u
    i32.const 0
    drop
    drop
  )

  (func $trunc_sat
    f32.const 42
    i32.trunc_sat_f32_s
    i32.const 0
    drop
    drop

    f32.const 42
    i32.trunc_sat_f32_u
    i32.const 0
    drop
    drop

    f64.const 42
    i32.trunc_sat_f64_s
    i32.const 0
    drop
    drop

    f64.const 42
    i32.trunc_sat_f64_u
    i32.const 0
    drop
    drop

    f32.const 42
    i64.trunc_sat_f32_s
    i32.const 0
    drop
    drop

    f32.const 42
    i64.trunc_sat_f32_u
    i32.const 0
    drop
    drop

    f64.const 42
    i64.trunc_sat_f64_s
    i32.const 0
    drop
    drop

    f64.const 42
    i64.trunc_sat_f64_u
    i32.const 0
    drop
    drop
  )

  (func $start
    call $trunc
    call $trunc_sat
  )

  (start $start)
)
