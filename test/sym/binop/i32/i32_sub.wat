(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)
    (local.set $x (call $i32_symbol))

    (i32.sub (local.get $x) (i32.const 1))
    i32.eqz
    (if (then unreachable))
  )

  (start $start)
)
