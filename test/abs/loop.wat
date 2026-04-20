(module
  (func $start
    i32.const 0
    (loop (param i32) (result i32)
      i32.const 2
      i32.add
      br 0)
    drop
  )

  (start $start)
)
