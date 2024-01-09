(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x f64)

    (local.set $x (call $f64_symbol))

    (f64.ge (f64.abs (local.get $x)) (f64.const 42))
    (if (then unreachable))

    (f64.le (f64.neg (local.get $x)) (f64.const 42))
    (if (then unreachable))
  )

  (start $start)
)
