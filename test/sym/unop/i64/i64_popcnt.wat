(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (i64.ge_s (i64.const 1) (i64.popcnt (local.get $i64)))
    (if (then unreachable))
  )

  (start $start)
)
