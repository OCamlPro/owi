(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $i32 i32)
    (local.set $i32 (call $i32_symbol))

    (f32.eq (f32.convert_i32_s (local.get $i32)) (f32.const 0))
    (if (then unreachable))
  )

  (start $start)
)
