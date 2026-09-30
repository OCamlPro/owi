(module
  (import "owi" "f64_symbol" (func $f64_symbol (result f64)))
  (import "owi" "assert" (func $assert (param i32)))

  (func $start
    (local $x f64)
    (local $y f64)

    (local.set $x (call $f64_symbol))
    (local.set $y (call $f64_symbol))

    (call $assert (f64.eq ;; False when x-y is NaN (because NaN != Nan)
      (f64.sub (local.get $x) (local.get $y))
      (f64.sub (local.get $x) (local.get $y))))
  )

  (start $start)
)
