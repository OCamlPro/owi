(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x i64)
    (local $y i64)
    (local.set $x (call $i64_symbol))
    (local.set $y (call $i64_symbol))

    (i64.eqz (i64.rotl (local.get $x) (local.get $y)))
    (if (then unreachable))
  )

  (start $start)
)
