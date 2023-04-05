(module
  (func $start
    i32.const 0
    i32.const 20
    i32.const 21
    i32.add
    i32.const 1
    i32.add
    i32.add
    drop

    i64.const 1
    i64.const 7
    i64.const 3
    i64.mul
    i64.const 2
    i64.mul
    i64.mul
    drop
  )
  (start $start)
)
