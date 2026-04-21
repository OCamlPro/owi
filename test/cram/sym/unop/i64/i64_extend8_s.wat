(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (i64.eq (i64.const 1) (i64.extend8_s (local.get $i64)))
    (if (then unreachable))
  )

  (start $start)
)
