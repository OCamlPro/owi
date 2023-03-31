(module
  (func $start
    i32.const 1
    ;; the following three lines should be
    ;; simplified to `i32.const 41`
    i32.const 20
    i32.const 21
    i32.add
    i32.add
    drop
  )
  (start $start)
)
