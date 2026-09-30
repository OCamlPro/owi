(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x i64)
    (local.set $x (call $i64_symbol))

    (i64.rem_u (i64.const 1) (local.get $x))
    i64.eqz
    (if (then unreachable))
  )

  (start $start)
)
