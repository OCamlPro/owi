(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (f64.eq (f64.const 42) (f64.reinterpret_i64 (local.get $i64)))
    (if (then unreachable))
  )

  (start $start)
)
