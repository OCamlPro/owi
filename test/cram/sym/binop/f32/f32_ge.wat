(module
  (import "owi" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $x f32)
    (local.set $x (call $f32_symbol))

    (f32.ge (local.get $x) (f32.const 1))
    (if (then unreachable))
  )

  (start $start)
)
