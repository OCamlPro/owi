(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))
  (memory 1)

  (func $f (param $x i32) (result i32)
    (local $value i32)

    (i32.store (i32.mul (i32.const 4) (i32.const 0)) (i32.const 1))
    (i32.store (i32.mul (i32.const 4) (i32.const 1)) (i32.const 2))
    (i32.store (i32.mul (i32.const 4) (i32.const 2)) (i32.const 0))
    (i32.store (i32.mul (i32.const 4) (i32.const 3)) (i32.const 4))

    (if (i32.ge_u (local.get $x) (i32.const 4) )
      (then (return (i32.const -1))))

    (local.set $value
      (i32.load
        (i32.mul
          (local.get $x)
          (i32.const 4))))

    (i32.div_s
      (i32.const 10)
      (local.get $value))
  )

  (func $check (export "check")
    call $i32_symbol
    call $f
    drop
  )
)
