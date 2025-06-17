(module
  (import "owi" "f32_symbol" (func $f32_symbol (result f32)))
  (import "owi" "assert" (func $assert (param i32)))

  (func $start
    (local $x f32)
    (local $y f32)

    (local.set $x (call $f32_symbol))
    (local.set $y (call $f32_symbol))

    (call $assert (f32.eq ;; False when x*y is NaN (because NaN != Nan)
      (f32.mul (local.get $x) (local.get $y))
      (f32.mul (local.get $x) (local.get $y))))
  )

  (start $start)
)
