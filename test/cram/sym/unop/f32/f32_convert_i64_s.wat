(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (f32.eq (f32.convert_i64_s (local.get $i64)) (f32.const 0))
    (if (then unreachable))
  )

  (start $start)
)
