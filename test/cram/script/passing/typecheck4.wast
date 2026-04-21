(module
  (func
    (block $b4 (result i32) (result f64)
      i32.const 0
      f64.const 0
      i32.const 0
      br_if $b4
      br $b4
      i32.const 0
      f64.const 0
    )
    drop
    drop
  )
)
