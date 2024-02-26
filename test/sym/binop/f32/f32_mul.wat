(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $x f32)
    (local $y f32)
    (local.set $x (call $f32_symbol))
    (local.set $y (call $f32_symbol))

    (f32.eq (f32.mul (local.get $x) (local.get $y)) (f32.const 1))
    (if (then unreachable))
  )

  (start $start)
)
