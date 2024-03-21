(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)
    (local.set $x (call $i32_symbol))

    (i32.rem_s (i32.const 1) (local.get $x))
    i32.eqz
    (if (then unreachable))
  )

  (start $start)
)
