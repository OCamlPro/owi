(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $f64 f64)
    (local.set $f64 (call $f64_symbol))

    (f64.le (f64.neg (local.get $f64)) (f64.const 0))
    (if (then unreachable))
  )

  (start $start)
)
