(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x i64)
    (local.set $x (call $i64_symbol))

    (i64.lt_s (local.get $x) (i64.const 1))
    (if (then unreachable))
  )

  (start $start)
)
