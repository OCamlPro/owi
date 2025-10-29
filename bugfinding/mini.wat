(module

  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start (local $x i32)
    (local.set $x (call $i32_symbol))

    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    ))
  )

  (start $start))
