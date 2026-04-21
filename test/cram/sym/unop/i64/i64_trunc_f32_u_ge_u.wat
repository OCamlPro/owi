(module
  (import "owi" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $f32 f32)
    (local.set $f32 (call $f32_symbol))

    (i64.ge_u (i64.const 1) (i64.trunc_f32_u (local.get $f32)))
    (if (then unreachable))
  )

  (start $start)
)
