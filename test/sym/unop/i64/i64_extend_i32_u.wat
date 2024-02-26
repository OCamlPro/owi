(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $i32 i32)
    (local.set $i32 (call $i32_symbol))

    (i64.eq (i64.const 1) (i64.extend_i32_u (local.get $i32)))
    (if (then unreachable))
  )

  (start $start)
)
