(module
  (func $br
    i32.const 42
    br 0
    i32.const 42
    i32.const 42
    drop
    drop
  )

  (func $br_if
    i32.const 0
    br_if 0
    i32.const 42
    br_if 0
    i32.const 42
    i32.const 42
    drop
    drop
  )

  (func $start
    call $br
    call $br_if
  )

  (start $start)
)
