(module
  (func $start
    i32.const 1
    (if
      (then nop)
      (else nop)
    )
    i32.const 0
    (if
      (then nop)
      (else nop)
    )
    i32.const 1
    (if (result i32)
      (then (i32.const 42))
      (else (i32.const 24))
    )
    drop
    i32.const 0
    (if (result i32)
      (then (i32.const 42))
      (else (i32.const 24))
    )
    drop
  )
  (start $start)
)
