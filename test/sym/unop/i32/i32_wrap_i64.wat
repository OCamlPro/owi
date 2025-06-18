(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $i64 i64)
    (local.set $i64 (call $i64_symbol))

    (i32.eq (i32.const 1) (i32.wrap_i64 (local.get $i64)))
    (if (then unreachable))
  )

  (start $start)
)
