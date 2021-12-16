(module
  (func $f (export "fibo") (param $n i32) (result i32)

    (if
      (i32.lt_s
        (local.get $n)
        (i32.const 2))
      (then (return (local.get $n))))
    (return
      (i32.add
        (call $f
          (i32.sub
            (local.get $n)
            (i32.const 2)))
        (call $f
          (i32.sub
            (local.get $n)
            (i32.const 1)))
      )
    )
  )
)

(assert_return (invoke "fibo" (i32.const 10)) (i32.const 55))
