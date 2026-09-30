(module
  (func $f
    i32.const 42
    (if (result i32) (then
      i32.const 12
    ) (else
      i32.const 24
    ))
    i32.const 0
    i32.mul
    unreachable
  )
)
