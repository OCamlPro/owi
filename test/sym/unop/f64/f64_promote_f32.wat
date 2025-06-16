(module
  (import "owi" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $f32 f32)
    (local.set $f32 (call $f32_symbol))

    (f64.eq (f64.promote_f32 (local.get $f32)) (f64.const 0))
    (if (then unreachable))
  )

  (start $start)
)
