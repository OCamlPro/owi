(module
  (func $i32relop
    i32.const 42
    i32.const 0
    i32.eq
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.ne
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.lt_s
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.lt_u
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.gt_s
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.gt_u
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.le_s
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.le_u
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.ge_s
    i32.const 0
    drop
    drop

    i32.const 42
    i32.const 0
    i32.ge_u
    i32.const 0
    drop
    drop
  )

  (func $i64relop
    i64.const 42
    i64.const 0
    i64.eq
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.ne
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.lt_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.lt_u
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.gt_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.gt_u
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.le_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.le_u
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.ge_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.const 0
    i64.ge_u
    i32.const 0
    drop
    drop
  )

  (func $f32relop
    f32.const 42
    f32.const 0
    f32.eq
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 0
    f32.ne
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 0
    f32.lt
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 0
    f32.gt
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 0
    f32.le
    i32.const 0
    drop
    drop

    f32.const 42
    f32.const 0
    f32.ge
    i32.const 0
    drop
    drop
  )

  (func $f64relop
    f64.const 42
    f64.const 0
    f64.eq
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 0
    f64.ne
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 0
    f64.lt
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 0
    f64.gt
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 0
    f64.le
    i32.const 0
    drop
    drop

    f64.const 42
    f64.const 0
    f64.ge
    i32.const 0
    drop
    drop
  )

  (func $itestop
    i32.const 0
    i32.eqz
    i32.const 0
    drop
    drop

    i64.const 0
    i64.eqz
    i32.const 0
    drop
    drop
  )

  (func $start
    call $i32relop
    call $i64relop
    call $f32relop
    call $f64relop
    call $itestop
  )

  (start $start)
)
