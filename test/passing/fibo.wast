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

(assert_return (invoke "fibo" (i32.const 0)) (i32.const 0))
(assert_return (invoke "fibo" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fibo" (i32.const 2)) (i32.const 1))
(assert_return (invoke "fibo" (i32.const 3)) (i32.const 2))
(assert_return (invoke "fibo" (i32.const 4)) (i32.const 3))
(assert_return (invoke "fibo" (i32.const 5)) (i32.const 5))
(assert_return (invoke "fibo" (i32.const 6)) (i32.const 8))
(assert_return (invoke "fibo" (i32.const 7)) (i32.const 13))
(assert_return (invoke "fibo" (i32.const 8)) (i32.const 21))
(assert_return (invoke "fibo" (i32.const 9)) (i32.const 34))
(assert_return (invoke "fibo" (i32.const 10)) (i32.const 55))
