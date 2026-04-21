(module
  (import "owi" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $i32 i32)
    (local.set $i32 (call $i32_symbol))

    (i32.ge_s (i32.const 1) (i32.popcnt (local.get $i32)))
    (if (then unreachable))
  )

  (start $start)
)
