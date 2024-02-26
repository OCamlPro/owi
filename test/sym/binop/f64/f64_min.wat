(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x f64)
    (local $y f64)
    (local.set $x (call $f64_symbol))
    (local.set $y (call $f64_symbol))

    (f64.eq (f64.min (local.get $x) (local.get $y)) (f64.const 1))
    (if (then unreachable))
  )

  (start $start)
)
