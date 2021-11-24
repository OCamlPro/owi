(module
  (func $f (export "f") (result i32)
    i32.const 0
  )
  (func $g (export "g") (result i32)
    i32.const 256
    i32.clz
    return
  )
  (func $h (export "h") (result f32)
    f32.const 9
    f32.sqrt
    return
  )
  (func $s (export "s") (result i32)
    i32.const 42
    i32.const 1
    i32.sub
    return
  )
  (func $stack (export "stack") (param $a i32) (param $b i32) (result i32)
    local.get 0
    local.get 1
    i32.sub
    return
  )
  (func $push2 (export "push2") (result i32) (result i32)
    i32.const 0
    i32.const 100
    return
  )

  (func $pushsub (export "pushsub") (result i32)
    call 5
    i32.sub
    return
  )

  (func $fibo (export "fibo") (param i32) (result i32)
    (if
      (i32.lt_s
        (local.get 0)
        (i32.const 2))
      (then (return (local.get 0))))
    (return
      (i32.add
        (call 7
          (i32.sub
            (local.get 0)
            (i32.const 2)))
        (call 7
          (i32.sub
            (local.get 0)
            (i32.const 1)))
      )
    )
  )

  (func $branch (export "branch") (param i32) (result i32)

    (if (local.get 0)
      (then
        (br 0) (return (i32.const 0)))
      (else
        (return (i32.const 1)))
    )
    i32.const 2
    return
  )

  (func $br_func (export "br_func") (param i32) (result i32)
    local.get 0
    br 0
  )
)

(assert_return (invoke "f") (i32.const 0))
(assert_return (invoke "g") (i32.const 23))
(assert_return (invoke "h") (f32.const 3))
(assert_return (invoke "s") (i32.const 41))

(assert_return (invoke "pushsub") (i32.const -100))
(assert_return (invoke "stack" (i32.const 2) (i32.const 39)) (i32.const -37))
(assert_return (invoke "fibo" (i32.const 10)) (i32.const 55))
(assert_return (invoke "branch" (i32.const 1)) (i32.const 2))

(assert_return (invoke "br_func" (i32.const 42)) (i32.const 42))
