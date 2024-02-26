(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $f32 f32)
    (local.set $f32 (call $f32_symbol))

    (f32.eq (f32.nearest (local.get $f32)) (f32.const 0))
    (if (then unreachable))
  )

  (start $start)
)
