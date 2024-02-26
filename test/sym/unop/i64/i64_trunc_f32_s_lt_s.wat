(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $f32 f32)
    (local.set $f32 (call $f32_symbol))

    (i64.lt_s (i64.const 1) (i64.trunc_f32_s (local.get $f32)))
    (if (then unreachable))
  )

  (start $start)
)
