(module
  (func $f (export "fibo") (param i32) (result i32)
    (local i32) (local i32)
    (local.set 2 (i32.const 1))
    (block (loop
        (br_if 1 (i32.eqz (local.get 0)))
        (i32.add (local.get 1) (local.get 2))
        local.set 2
        (i32.sub (local.get 2) (local.get 1))
        local.set 1
        (i32.sub (local.get 0) (i32.const 1))
        local.set 0
        br 0
    ))
    local.get 1
    return
  )
)

(assert_return (invoke "fibo" (i32.const 10)) (i32.const 55))

(module
  (func $g (export "fibo2") (param i32) (result i32)
    (local i32) (local i32)

    i32.const 0
    (i32.eqz (local.get 0))
    br_if 0
    drop ;; optional

    (local.set 2 (i32.const 1))

    (loop
        (i32.add (local.get 1) (local.get 2))
        local.set 2
        (i32.sub (local.get 2) (local.get 1))
        local.set 1
        (i32.sub (local.get 0) (i32.const 1))
        local.set 0

        (br_if 0 (i32.gt_s (local.get 0) (i32.const 0)))
    )
    local.get 1
    return
  )
)

(assert_return (invoke "fibo2" (i32.const 10)) (i32.const 55))

(assert_return (invoke "fibo2" (i32.const 0)) (i32.const 0))
