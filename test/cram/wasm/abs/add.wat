(module
  (import "owi" "assert" (func $assert (param i32)))

  (func $start
    i32.const 42
    i32.const 28
    i32.sub
    i32.const 47
    i32.add
    i64.extend_i32_s
    i64.const 100
    i64.add
    i64.const 200
    i64.gt_s
    return
  )

  (start $start)
)
