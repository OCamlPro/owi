(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (f64.eq (f64.convert_i64_u (local.get $i64)) (f64.const 0))
    (if (then unreachable))
  )

  (start $start)
)
