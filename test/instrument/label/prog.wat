(module
  (func $f
    i32.const 42
    (if (then
      i32.const 12
      drop
    ) (else
      i32.const 24
      drop
    ))
    i32.const 0
    i32.mul
    unreachable
  )
)
