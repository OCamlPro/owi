(module
  (func $extend
    i32.const 42
    i32.extend8_s
    i32.const 0
    drop
    drop

    i32.const 42
    i32.extend16_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.extend8_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.extend16_s
    i32.const 0
    drop
    drop

    i64.const 42
    i64.extend32_s
    i32.const 0
    drop
    drop

    i32.const 42
    i64.extend_i32_s
    i32.const 0
    drop
    drop

    i32.const 42
    i64.extend_i32_u
    i32.const 0
    drop
    drop
  )

  (func $wrap
    i64.const 42
    i32.wrap_i64
    i32.const 0
    drop
    drop
  )

  (func $start
    call $extend
    call $wrap
  )

  (start $start)
)
