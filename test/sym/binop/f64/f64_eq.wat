(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x f64)
    (local.set $x (call $f64_symbol))

    (f64.eq (local.get $x) (f64.const 1))
    (if (then unreachable))
  )

  (start $start)
)
