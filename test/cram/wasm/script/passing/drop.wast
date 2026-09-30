(module $m
  (memory $m0 6)
  (func $start
    (block $b0  (result i32)
      i32.const 42
      br $b0
      i32.eqz
      memory.size
      (if
        (then
          i64.const 0
          drop
        )
      )
      memory.grow
      drop
      drop
      i32.const 0
    )
    drop
  )
)
