(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $i32 i32)
    (local.set $i32 (call $i32_symbol))

    (f64.eq (f64.convert_i32_u (local.get $i32)) (f64.const 0))
    (if (then unreachable))
  )

  (start $start)
)
