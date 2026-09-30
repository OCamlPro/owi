(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)
    (local $y i32)
    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))

    (i32.eqz (i32.rotl (local.get $x) (local.get $y)))
    (if (then unreachable))
  )

  (start $start)
)
