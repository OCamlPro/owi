(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (i64.eqz (local.get $i64))
    (if (then unreachable))
  )

  (start $start)
)
